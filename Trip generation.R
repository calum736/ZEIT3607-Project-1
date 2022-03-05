#---- PARAMETERS ----
FT_STAFF <- 0.80
PT_STAFF <- 0.20

PG_STUDENT <- 0.60
UG_STUDENT <- 0.40

STUDENT_CNT <- 5000
STAFF_CNT <- 700

BASE_YEAR <- 2020
TARGET_YEAR <- 2040
TIME_GROWTH <- 0.15 # travel times expected to increase 15% in target year

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

#---- EXTRACT GROUP 3 SAMPLE ----
student_sample <- filter(student_data, student_data$ID %in% student_IDs$ID)
staff_sample <- filter(staff_data, staff_data$ID %in% staff_IDs$ID)






# Write sample set to CSV for future reference
write.csv(student_sample, "Group_3_Student_Data.csv")
write.csv(staff_sample, "Group_3_Staff_Data.csv")

student_sample %>%
  #filter(Day == "Wednesday") %>%
  ggplot() +
  geom_bar(aes(Entrance, fill = Day)) +
  facet_wrap("Day")

