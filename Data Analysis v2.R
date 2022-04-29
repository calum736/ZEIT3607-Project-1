


#---- PACKAGES ----
library(tidyverse)
library(janitor)

#---- UTILITY FUNCTIONS -----
x_prod_list <- function(A, B) {
  x_list <- vector(mode = "numeric", len = 0) # should mode be "list"?
  for (i in 1:length(A)) {
    for (j in 1:length(B)) {
      key <- paste(names(A)[[i]], names(B)[[j]], sep = "_")
      value <- A[[i]] * B[[j]]
      print(key)
      print(value)
      x_list[[key]] <- value
    }
  }
  return(x_list)
}

#----------------------------------------------#
#--- LOAD SAMPLE DATA -------------------------#
#----------------------------------------------#

setwd("~/ZEIT3607/Project 1")

#- Read student and staff data from master file
student_master <- read.csv(file = "data/1_Students.csv")
staff_master <- read.csv(file = "data/1_Staff.csv")

student_IDs <- read.csv(file = "data/Group_3_StudentIDs.csv")
staff_IDs <- read.csv(file = "data/Group_3_StaffIDs.csv")

#----------------------------------------------#
#--- 1. TRIP GENERATION -----------------------#
#----------------------------------------------#

#- Pull out a subset of data from the sample list
student_sample <-
  filter(student_master, student_master$ID %in% student_IDs$ID)
staff_sample <-
  filter(staff_master, staff_master$ID %in% staff_IDs$ID)

n_students <- n_distinct(student_sample$ID)
n_staff <- n_distinct(staff_sample$ID)

#- Average student weekly trips by school, pg/ug and gender
av_trips_student <- student_sample %>%
  group_by(ID, School, Career) %>%
  summarise(WeeklyTrips = n()) %>%
  group_by(School, Career) %>%
  summarise("AverageTrips" = mean(WeeklyTrips)) %>%
  mutate(ID = paste(School, Career, sep = "_"),
         .before = School)

#- Average staff weekly trips by School, pt/ft and role
av_trips_staff <- staff_sample %>%
  group_by(ID, School, FulltimeParttime) %>%
  summarise(WeeklyTrips = n()) %>%
  group_by(School, FulltimeParttime) %>%
  summarise("AverageTrips" = mean(WeeklyTrips)) %>%
  mutate(ID = paste(School, FulltimeParttime, sep = "_"),
         .before = School)


#- Define the parameters for the new city campus (as given in the brief)
N_STUDENT <- 5000
N_STAFF <- 700
CAREER <- c("PGRD" = 0.60,
            "UGRD" = 0.40)
GENDER <- c("M" = 0.50,
            "F" = 0.50)
POSITION_TITLE <- c(
  "admin" = 0.10,
  "executive" = 0.10,
  "director" = 0.05,
  "professor" = 0.05,
  "associate_professor" = 0.05,
  "senior_lecturer" = 0.15,
  "lecturer" = 0.15,
  "research_fellow" = 0.25,
  "technical_support" = 0.10
)
CONTRACT_HRS <- c("Full-Time" = 0.80,
                  "Part-Time" = 0.20)
SCHOOL <- c(
  "SEIT" = 0.40,
  "HASS" = 0.20,
  "BUS" = 0.20,
  "SCI" = 0.20
)

#- Calculate city student populations
city_student_pop <-
  enframe(x_prod_list(SCHOOL, CAREER),
          name = "ID",
          value = "Weight") %>%
  mutate(n = Weight * N_STUDENT)

#- Calculate city staff populations
city_staff_pop <-
  enframe(x_prod_list(SCHOOL, CONTRACT_HRS),
          name = "ID",
          value = "Weight") %>%
  mutate(n = Weight * N_STAFF)

#- Calculate average trips per week day (cross-classification method)
city_student_pop <- city_student_pop %>%
  full_join(av_trips_student, by = "ID") %>%
  mutate(TotalTrips = n * AverageTrips / 5)
city_staff_pop <- city_staff_pop %>%
  full_join(av_trips_staff, by = "ID") %>%
  mutate(TotalTrips = n * AverageTrips / 5)

write.csv(city_student_pop, "output data/city_student_pop.csv")
write.csv(city_staff_pop, "output data/city_staff_pop.csv")

city_total_trips = sum(city_student_pop$TotalTrips, na.rm = TRUE) + sum(city_staff_pop$TotalTrips, na.rm =
                                                                          TRUE) #remove NAs

# TODO: need to factor in that the av. weekly trips is counting weekends



#Details for the City Campus
city_zone_data <- data.frame(
  Zone = c(
    "Belconnen",
    "Gungahlin",
    "Inner North",
    "Inner South",
    "Woden",
    "West Creek",
    "Tuggeranong",
    "Queanbeyan"
  ),
  Population = c(100040, 83167, 58702, 27618, 34551, 20611, 82649, 61031),
  Distance_City = c(13.30, 14.91, 5.70, 6.00, 11.00, 15.90, 19.40, 14.70),
  Auto_TT_mean = c(17, 31, 12, 12, 15, 23, 29, 25),
  PT_TT_min = c(31, 51, 23, 19, 36, 32, 48, 34),
  PT_TT_max = c(38, 71, 34, 33, 56, 47, 72, 49),
  PT_TT_mean = c(33, 62, 29, 27, 49, 42, 64, 44)
)

calc_zone_trips <-
  function(zone_data,
           total_trips,
           Auto_Cost_KM,
           PT_Cost) {
    #----------------------------------------------#
    #--- 2. TRIP DISTRIBUTION ---------------------#
    #----------------------------------------------#
    
    #---- Gravity ----
    zone_data <-
      zone_data %>%
      mutate(
        # This tutorial uses distance as the generalised cost
        FrictionFactor = Distance_City ^ (-0.41) / 0.41,
        # This tutorial assumes alpha to be 0.1
        Alpha = 2.5514E-6,
        # The gravity model:
        Trips = Alpha * Population * total_trips * FrictionFactor
      )
    
    #- calibration debug
    #Total trip production:
    zone_data$Trips %>% sum()
    #Total trip attraction
    city_total_trips
    
    #---- Visualisation ----
    print(zone_data %>% select(Zone, Trips))
    
    zone_data %>%
      ggplot() +
      geom_col(aes(x = reorder(Zone,-Trips), y = Trips)) +
      labs(x = "Zone") +
      theme_bw()
    
    zone_data %>%
      select(Zone, Trips, Distance_City, Population) %>%
      mutate(Zone = reorder(Zone,-Trips)) %>%
      gather(-Zone, key = Measure, value = Value) %>%
      ggplot() +
      geom_col(aes(x = Zone, y = Value)) +
      facet_grid(rows = vars(Measure), scales = "free") +
      theme_bw()
    
    zone_data %>%
      ggplot() +
      geom_col(aes(
        x = reorder(Zone,-Trips),
        y = Trips,
        fill = Population
      )) +
      labs(x = "Zone") +
      theme_bw()
    
    #----------------------------------------------#
    #--- 3. Mode Choice ---------------------------#
    #----------------------------------------------#
    
    #---- Utilities ----
    zone_data <-
      zone_data %>%
      mutate(
        PT_Reliability = (PT_TT_max - PT_TT_min) / PT_TT_mean,
        Auto_Cost = Auto_Cost_KM * Distance_City
      ) %>%
      #Assuming three modes are available
      #Calculating utilities for students
      mutate(
        U_Auto = -0.25 * Auto_TT_mean - 0.11 * Auto_Cost,
        U_PT = -0.26 - 0.15 * PT_TT_mean - 0.13 * PT_Cost - 0.01 * PT_Reliability,
        U_Active = 0.1 - 0.67 * Distance_City
      )
    
    #---- Probabilities  ----
    zone_data <-  zone_data %>%
      mutate(
        Exp_U_Auto = exp(U_Auto),
        Exp_U_PT = exp(U_PT),
        Exp_U_Active = exp(U_Active),
        Exp_Sum = Exp_U_Auto + Exp_U_PT + Exp_U_Active,
        
        Prob_Auto = Exp_U_Auto / Exp_Sum,
        Prob_PT = Exp_U_PT / Exp_Sum,
        Prob_Active = Exp_U_Active / Exp_Sum,
        
        Trips_Auto = Trips * Prob_Auto,
        Trips_PT = Trips * Prob_PT,
        Trips_Active = Trips * Prob_Active
      )
    
    #---- Visualisation  ----
    zone_data %>%
      select(Zone, contains("Prob")) %>%
      mutate(Zone = reorder(Zone,-Prob_Auto)) %>%
      gather(-Zone, key = Mode, value = Probability) %>%
      mutate(Mode = ordered(Mode, levels = c("Prob_Active", "Prob_PT", "Prob_Auto"))) %>%
      ggplot() +
      geom_col(aes(x = Zone, y = Probability, fill = Mode)) +
      theme_bw()
    
    return (zone_data)
  }

Auto_Cost_KM = 0.35 * 2  # ATO $0.7/km * 2 since return trip


zone_trips_student <- calc_zone_trips(city_zone_data,
                                      sum(city_student_pop$TotalTrips, na.rm = TRUE),
                                      Auto_Cost_KM,
                                      1.66)
zone_trips_staff <- calc_zone_trips(city_zone_data,
                                   sum(city_staff_pop$TotalTrips, na.rm = TRUE),
                                   Auto_Cost_KM,
                                   3.22)
zone_trips_total <- rbind(zone_trips_student, zone_trips_staff) %>% 
  select(Zone, contains("Trips")) %>% 
  group_by(Zone) %>% 
  summarise_all(sum) %>% 
  adorn_totals(name = 'TOTAL')

#---- Write output to file and print to console  ----
write.csv(
  zone_trips_total,
  "output data/predicted_trips_by_zone_mode.csv"
)

print(zone_trips_total)
