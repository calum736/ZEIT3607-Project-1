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

multiply_df_weights <- function(...) {
  input <- list(...)
  output <- input[[1]][1]
  for (i in 2:length(input)) {
    output <- merge(output, input[[i]][1])
  }
  #output$ID <- do.call(paste, output[names(output)])
  input_length <- length(input)
  #output <- output[, c(input_length, (1:ncol(output))[-input_length])]
  print(output)
  for (i in 1:length(input)) {
    output <- merge(output, input[[i]], by = names(output)[i])
  }
  output_length <- length(output)
  output$Combined_Weight = Reduce('*', output[, (1 + input_length):output_length])
  output <- output[-c((input_length+1):output_length)]
  print(output)
  return(output)
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

#----- Pull out a subset of data from the sample list----
student_sample <-
  filter(student_master, student_master$ID %in% student_IDs$ID)
staff_sample <-
  filter(staff_master, staff_master$ID %in% staff_IDs$ID)

n_students <- n_distinct(student_sample$ID)
n_staff <- n_distinct(staff_sample$ID)

#---- Average student weekly trips ----
av_trips_student <- student_sample %>%
  group_by(ID, School, Career) %>%
  summarise(WeeklyTrips = n()) %>%
  group_by(School, Career) %>%
  summarise("AverageTrips" = mean(WeeklyTrips)) %>%
  mutate(ID = paste(School, Career, sep = " "),
         .before = School)

#---- Average staff weekly trips ----
av_trips_staff <- staff_sample %>%
  group_by(ID, School, FulltimeParttime) %>%
  summarise(WeeklyTrips = n()) %>%
  group_by(School, FulltimeParttime) %>%
  summarise("AverageTrips" = mean(WeeklyTrips)) %>%
  mutate(ID = paste(School, FulltimeParttime, sep = " "),
         .before = School)

#- Regression model
student_weekly_trips <-
  student_sample %>%
  group_by(ID) %>%
  summarise(
    WeeklyTrips = n(),
    School = first(School),
    Career = first(Career),
    Gender = first(Gender)
  )
student_weekly_model <-
  lm(WeeklyTrips ~ School + Career, data = student_weekly_trips)
print(summary(student_weekly_model))

staff_weekly_trips <-
  staff_sample %>%
  group_by(ID) %>%
  summarise(
    WeeklyTrips = n(),
    School = first(School),
    FulltimeParttime = first(FulltimeParttime),
    Level = first(Level)
  )
staff_weekly_model <-
  lm(WeeklyTrips ~ School + FulltimeParttime, data = staff_weekly_trips)
print(summary(staff_weekly_model))



#- Define the parameters for the new city campus (as given in the brief)
N_STUDENT <- 5000
N_STAFF <- 700
CAREER <- data.frame(Career = c("PGRD", "UGRD"),
                     Weight = c(0.6, 0.4))
GENDER <- data.frame(Gender = c("M", "F"),
                     Weight = c(0.5, 0.5))
POSITION_TITLE <- data.frame(
  PositionTitle = c(
    "admin",
    "executive",
    "director",
    "professor",
    "associate_professor",
    "senior_lecturer",
    "lecturer",
    "research_fellow",
    "technical_support"
  ),
  Weight = c(0.10, 0.10, 0.05, 0.05, 0.05, 0.15, 0.15, 0.25, 0.10)
)
CONTRACT_HRS <-
  data.frame(
    FulltimeParttime = c("Full-Time", "Part-Time"),
    Weight = c(0.80, 0.20)
  )
SCHOOL <- data.frame(
  School = c("SEIT", "HASS", "BUS", "SCI"),
  Weight = c(0.40, 0.20, 0.20, 0.20)
)
#- Calculate city student populations
city_student_pop <-
  multiply_df_weights(SCHOOL, CAREER) %>% 
  mutate(n = Combined_Weight * N_STUDENT)  %>%
  mutate(ID = paste(School, Career, sep = " "),
         .before = Career)

#- Calculate city staff populations
city_staff_pop <-
  multiply_df_weights(SCHOOL, CONTRACT_HRS) %>% 
  mutate(n = Combined_Weight * N_STAFF)  %>%
  mutate(ID = paste(School, FulltimeParttime, sep = " "),
         .before = FulltimeParttime)
  

city_student_pop$TripRate <- predict(student_weekly_model, city_student_pop)
city_staff_pop$TripRate <- predict(staff_weekly_model, city_staff_pop)

#- Calculate average trips per week day (cross-classification method)
city_student_pop <- city_student_pop %>%
  full_join(av_trips_student, by = "ID") %>%
  mutate(TotalTrips = n * TripRate / 5)
city_staff_pop <- city_staff_pop %>%
  full_join(av_trips_staff, by = "ID") %>%
  mutate(TotalTrips = n * TripRate / 5)

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
  #Population = c(100040, 83167, 58702, 27618, 34551, 20611, 82649, 61031), # 2020 population
  Population = c(152452, 87507, 91597, 38530, 56363, 26268, 90178, 78756),
  # 2040 population projection
  Distance_City = c(13.30, 14.91, 5.70, 6.00, 11.00, 15.90, 19.40, 14.70),
  Auto_TT_mean = c(17, 31, 12, 12, 15, 23, 29, 25),
  PT_TT_min = c(31, 51, 23, 19, 36, 32, 48, 34),
  PT_TT_max = c(38, 71, 34, 33, 56, 47, 72, 49),
  PT_TT_mean = c(33, 62, 29, 27, 49, 42, 64, 44),
  ODT_TT_min = c(20, 38, 16, 14, 22, 24, 35, 27),
  ODT_TT_max = c(31, 49, 24, 26, 36, 39, 57, 41),
  ODT_TT_mean = c(23, 43, 20, 21, 30, 31, 49, 34)
)


#--- Function to calculate trip dist and mode choice ----
calc_zone_trips <-
  function(zone_data,
           total_trips,
           Auto_Cost_KM,
           PT_Cost,
           ODT_Cost) {
    #----------------------------------------------#
    #--- 2. TRIP DISTRIBUTION ---------------------#
    #----------------------------------------------#
    
    #---- Gravity ----
    zone_data <-
      zone_data %>%
      mutate(
        # This tutorial uses distance as the generalised cost
        FrictionFactor = Distance_City ^ (-0.41) / 0.41,
        #Alpha = 2.5514E-6, #2020 population
        Alpha = 2.0564E-6,
        # The gravity model:
        Trips = Alpha * Population * total_trips * FrictionFactor
      )
    
    #- calibration debug
    #Total trip production:
    zone_data_total_trips <- zone_data$Trips %>% sum()
    print(zone_data_total_trips)
    #Total trip attraction
    print(city_total_trips)
    
    #---- Visualisation ----
    print(zone_data %>% select(Zone, Trips))
    
    print(zone_data %>%
            ggplot() +
            geom_col(aes(
              x = reorder(Zone, -Trips), y = Trips
            )) +
            labs(x = "Zone") +
            theme_bw())
    
    print(
      zone_data %>%
        select(Zone, Trips, Distance_City, Population) %>%
        mutate(Zone = reorder(Zone, -Trips)) %>%
        gather(-Zone, key = Measure, value = Value) %>%
        ggplot() +
        geom_col(aes(x = Zone, y = Value)) +
        facet_grid(rows = vars(Measure), scales = "free") +
        theme_bw()
    )
    
    print(zone_data %>%
            ggplot() +
            geom_col(aes(
              x = reorder(Zone, -Trips),
              y = Trips,
              fill = Population
            )) +
            labs(x = "Zone") +
            theme_bw())
    
    #----------------------------------------------#
    #--- 3. Mode Choice ---------------------------#
    #----------------------------------------------#
    
    #---- Utilities ----
    zone_data <-
      zone_data %>%
      mutate(
        PT_Reliability = (PT_TT_max - PT_TT_min) / PT_TT_mean,
        ODT_Reliability = (ODT_TT_max - ODT_TT_min) / ODT_TT_mean,
        Auto_Cost = Auto_Cost_KM * Distance_City #+ 5 #5 dollar parking for proposal 2
      ) %>%
      #Assuming three modes are available
      #Calculating utilities for students
      mutate(
        U_Auto = -0.25 * Auto_TT_mean - 0.11 * Auto_Cost,
        U_PT = -0.26 - 0.15 * PT_TT_mean - 0.13 * PT_Cost - 0.01 * PT_Reliability,
        U_ODT = -0.26 - 0.15 * ODT_TT_mean - 0.13 * ODT_Cost - 0.01 * ODT_Reliability,
        #assume same utility function for odt
        U_Active = 0.1 - 0.67 * Distance_City
      )
    
    #---- Probabilities  ----
    zone_data <-  zone_data %>%
      mutate(
        Exp_U_Auto = exp(U_Auto),
        Exp_U_PT = exp(U_PT),
        Exp_U_ODT = exp(U_ODT),
        Exp_U_Active = exp(U_Active),
        Exp_Sum = Exp_U_Auto + Exp_U_PT + Exp_U_ODT + Exp_U_Active,
        
        Prob_Auto = Exp_U_Auto / Exp_Sum,
        Prob_PT = Exp_U_PT / Exp_Sum,
        Prob_ODT = Exp_U_ODT / Exp_Sum,
        Prob_Active = Exp_U_Active / Exp_Sum,
        
        Trips_Auto = Trips * Prob_Auto,
        Trips_PT = Trips * Prob_PT,
        Trips_ODT = Trips * Prob_ODT,
        Trips_Active = Trips * Prob_Active
      )
    
    #---- Visualisation  ----
    print(
      zone_data %>%
        select(Zone, contains("Prob")) %>%
        mutate(Zone = reorder(Zone, -Prob_Auto)) %>%
        gather(-Zone, key = Mode, value = Probability) %>%
        mutate(Mode = ordered(
          Mode, levels = c("Prob_Active", "Prob_PT", "Prob_Auto")
        )) %>%
        ggplot() +
        geom_col(aes(
          x = Zone, y = Probability, fill = Mode
        )) +
        theme_bw()
    )
    
    return (zone_data)
  }


# ---- Determine trips ----
Auto_Cost_KM = 0.35 * 2  # ATO $0.7/km * 2 since return trip
TT_growth = 1.15 # TT for auto and pt expected to grow by 15% in 2040
city_zone_data <-
  city_zone_data %>% mutate_at(vars(Auto_TT_mean, PT_TT_min, PT_TT_max, PT_TT_mean),
                               .funs = funs(. * TT_growth))

city_trips_student <- sum(city_student_pop$TotalTrips, na.rm = TRUE)
zone_trips_student <- calc_zone_trips(
  city_zone_data,
  city_trips_student,
  Auto_Cost_KM,
  1.66 * 2,
  1.66 * 2 * 1.5
)

city_trips_staff <- sum(city_staff_pop$TotalTrips, na.rm = TRUE)
zone_trips_staff <- calc_zone_trips(
  city_zone_data,
  city_trips_staff,
  Auto_Cost_KM,
  3.22 * 2,
  3.22 * 2 * 1.5
)

zone_trips_total <- rbind(zone_trips_student, zone_trips_staff) %>%
  select(Zone, contains("Trips")) %>%
  group_by(Zone) %>%
  summarise_all(sum) %>%
  adorn_totals(name = 'TOTAL')

#---- Write output to file and print to console  ----
write.csv(zone_trips_total,
          "output data/predicted_trips_by_zone_mode.csv")

print(zone_trips_total)
