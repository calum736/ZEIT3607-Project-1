#---- MODEL PARAMETERS ----
FT_STAFF <- 0.80
PT_STAFF <- 0.20

# Job title expected composition as per brief
# Unsure, better to use this or use proportion of people by faculty???
STAFF_BREAKDOWN <- c(
  "admin"=0.10,
  "executive"=0.10,
  "director"=0.05,
  "professor"=0.05,
  "associate_professor"=0.05,
  "senior_lecturer"=0.15,
  "lecturer"=0.15,
  "research_fellow"=0.25,
  "technical_support"=0.10
)

FACULTY_BREAKDOWN <- c(
  "SEIT"=0.40,
  "HASS"=0.20,
  "BUS"=0.20,
  "SCI"=0.20
)
    
# Staff hrs schedule see page 51 of EA (too much detail, ignore for now)
# https://www.hr.unsw.edu.au/services/indrel/The%20University%20of%20New%20South%20Wales%20(Professional%20Staff)%20Enterprise%20Agreement%202018.pdf

PG_STUDENT <- 0.60
UG_STUDENT <- 0.40

STUDENT_CNT <- 5000
STAFF_CNT <- 700

BASE_YEAR <- 2020
TARGET_YEAR <- 2040
TRIP_DURATION_GROWTH <- 0.15 # travel times expected to increase 15% in target year

#---- PACKAGES ----
library(tidyverse)

#---- READ CSV DATA ----
student_data <- read.csv(file = 'data/1_Students.csv')
staff_data <- read.csv(file = 'data/1_Staff.csv')

student_IDs <- read.csv(file = 'data/Group_3_StudentIDs.csv')
staff_IDs <- read.csv(file = 'data/Group_3_StaffIDs.csv')

#---- GET COUNTS ----
ADFA_STUDENT_CNT <- n_distinct(student_data$ID)
ADFA_STAFF_CNT <- n_distinct(staff_data$ID)

#---- EXTRACT GROUP 3 RECORDS ----
student_sample <- filter(student_data, student_data$ID %in% student_IDs$ID)
staff_sample <- filter(staff_data, staff_data$ID %in% staff_IDs$ID)

# Write sample set to CSV for future reference
write.csv(student_sample, "Group_3_Student_Data.csv")
write.csv(staff_sample, "Group_3_Staff_Data.csv")

# Plot sample - example from week 1 tut
# TODO find out if ggplot has a Sankey chart to illustrate traffic flow
student_sample %>%
  #filter(Day == "Wednesday") %>%
  ggplot() +
  geom_bar(aes(Entrance, fill = Day)) +
  facet_wrap("Day")

#AIM 
#Need to predict trip data based on adfa campus data and above parameters

#TODO
#Flow in versus flow out - plot cumulative curve to find periods when most busy.
a <- student_sample %>% 
        group_by(Day, Entrance) %>% 
        summarise(n1 = n()) %>% 
        unite(day_time, Day:Entrance, remove = FALSE)
b <- student_sample %>%
        group_by(Day, Exit) %>% 
        summarise(n2 = n()) %>% 
        unite(day_time, Day:Exit, remove = FALSE)
c <- merge(a, b, by="day_time")


#Analyse the adfa data, establish rates
#Apply rates and parameters to new campus population

