#------------------------------#
#- ZEIT3607 - Tutorial 3 ------#
#- Trip Generation ------------#
#- 11 March 2022 --------------#
#- Milad Ghasri ---------------#
#------------------------------#

#- packages -----
library(tidyverse)

#- Setting up directory ----
getwd()
setwd("~/ZEIT3607/Project 1")

#- Reading data -----
student_master <- read.csv(file = "data/1_Students.csv")
staff_master <- read.csv(file = "data/1_Staff_Modified.csv")

student_IDs <- read.csv(file = "data/Group_3_StudentIDs.csv")
staff_IDs <- read.csv(file = "data/Group_3_StaffIDs.csv")

#----------------------------------------------#
#--- 1. TRIP GENERATION -----------------------#
#----------------------------------------------#

#----- Pull out a subset of data from the sample list----
Data_Student <-
  filter(student_master, student_master$ID %in% student_IDs$ID)
#Data_Student <-
#  filter(staff_master, staff_master$ID %in% staff_IDs$ID)


str(Data_Student)  


#- Trip generation ----
Data_Student_Weekly <- 
  Data_Student %>% 
  group_by(ID) %>% 
  summarise(WeeklyTrips=n(), School=first(School), Career=first(Career), Gender=first(Gender))

Student_Weekly_Model <- lm(WeeklyTrips ~ Career + School + Gender , data = Data_Student_Weekly)
summary(Student_Weekly_Model)


#- outsample validation ----
Data_Student_Weekly_Train <- 
  Data_Student_Weekly %>% 
  sample_frac(0.8)

Data_Student_Weekly_Test <- 
  Data_Student_Weekly %>% 
  filter(!(ID %in% Data_Student_Weekly_Train$ID))

Model_Student <- lm(WeeklyTrips~School, data=Data_Student_Weekly_Train)
summary(Model_Student)

Predicted_trips <- predict(Model_Student, newdata = Data_Student_Weekly_Test)

Data_Student_Weekly_Test <- 
  Data_Student_Weekly_Test %>% 
  cbind(Predicted_trips)


Data_Student_Weekly_Test %>% 
  ggplot()+
  geom_point(aes(x=Predicted_trips, y=WeeklyTrips), position = "jitter")


Data_Student_Weekly_Test %>% 
  mutate(Difference = (Predicted_trips - WeeklyTrips)^2) %>% 
  group_by() %>% 
  summarise(RMSE = (sum(Difference)/nrow(Data_Student_Weekly_Test))^0.5)

#- Random visualisation ----

Data_Student_Weekly %>% 
  group_by(School) %>% 
  summarise(AverageTrips = mean(WeeklyTrips)) %>% 
  ggplot()+
  geom_col(aes(x=School, y=AverageTrips)) +
  theme_bw()

#- Calculating peak day ratio ----
Data_Student
