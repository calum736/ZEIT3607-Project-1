#- Packages ----
library(tidyverse)

#- Read data ----
student_master <- read.csv(file = "data/1_Students.csv")
staff_master <- read.csv(file = "data/1_Staff.csv")

student_IDs <- read.csv(file = "data/Group_3_StudentIDs.csv")
staff_IDs <- read.csv(file = "data/Group_3_StaffIDs.csv")

#- Pull group 3 data -----
Data_Student <- filter(student_master, ID %in% student_IDs$ID)
Data_Staff <- filter(staff_master, ID %in% staff_IDs$ID)

#- Trip generation ----
Data_Student_Weekly <- 
  Data_Student %>% 
  group_by(ID) %>% 
  summarise(WeeklyTrips=n(), School=first(School), Career=first(Career), Gender=first(Gender))
write.csv(Data_Student_Weekly, 'output data/Data_Student_weekly_av_trips.csv')

Data_Staff_Weekly <- 
  Data_Staff %>% 
  group_by(ID) %>% 
  summarise(WeeklyTrips=n(), School=first(School), Level=first(Level), FulltimeParttime=first(FulltimeParttime))
write.csv(Data_Staff_Weekly, 'output data/Data_Student_weekly_av_trips.csv')
# Data_1 <-
#   Data_Student %>% 
#   group_by(ID, School, Career) %>% 
#   summarise(n = n()) 
#   

# Student_Weekly_Model <- lm(WeeklyTrips~School+Career+Gender, data = Data_Student_Weekly)
# summary(Student_Weekly_Model)
# par(mfrow = c(2, 2))
# plot(Student_Weekly_Model)
# 
# #plot(Student_Weekly_Model)
# 
# #http://www.sthda.com/english/articles/40-regression-analysis/163-regression-with-categorical-variables-dummy-coding-essentials-in-r/
# 
# #- outsample validation ----
# Data_Student_Weekly_Train <- 
#   Data_Student_Weekly %>% 
#   sample_frac(0.8)
# 
# Data_Student_Weekly_Test <- 
#   Data_Student_Weekly %>% 
#   filter(!(ID %in% Data_Student_Weekly_Train$ID))
# 
# Model_Student <- lm(WeeklyTrips~School+Career+Gender, data=Data_Student_Weekly_Train)
# summary(Model_Student)
# 
# Predicted_trips <- predict(Model_Student, newdata = Data_Student_Weekly_Test)
# 
# Data_Student_Weekly_Test <- 
#   Data_Student_Weekly_Test %>% 
#   cbind(Predicted_trips)
# 
# 
# Data_Student_Weekly_Test %>% 
#   ggplot()+
#   geom_point(aes(x=Predicted_trips, y=WeeklyTrips), position = "jitter")
# 
# 
# Data_Student_Weekly_Test %>% 
#   mutate(Difference = (Predicted_trips - WeeklyTrips)^2) %>% 
#   group_by() %>% 
#   summarise(RMSE = (sum(Difference)/nrow(Data_Student_Weekly_Test))^0.5)


Staff_Weekly_Model <- lm(WeeklyTrips~School+Level+FulltimeParttime, data = Data_Staff_Weekly)
anova(Staff_Weekly_Model)

par(mfrow = c(2, 2))
plot(Staff_Weekly_Model)

#- outsample validation ----
Data_Staff_Weekly_Train <- 
  Data_Staff_Weekly %>% 
  sample_frac(0.8)

Data_Staff_Weekly_Test <- 
  Data_Staff_Weekly %>% 
  filter(!(ID %in% Data_Staff_Weekly_Train$ID))

Model_Staff <- lm(WeeklyTrips~School+Level+FulltimeParttime, data=Data_Staff_Weekly_Train)
summary(Model_Staff)

Predicted_trips <- predict(Model_Staff, newdata = Data_Staff_Weekly_Test)

Data_Staff_Weekly_Test <- 
  Data_Staff_Weekly_Test %>% 
  cbind(Predicted_trips)


Data_Staff_Weekly_Test %>% 
  ggplot()+
  geom_point(aes(x=Predicted_trips, y=WeeklyTrips), position = "jitter")


Data_Staff_Weekly_Test %>% 
  mutate(Difference = (Predicted_trips - WeeklyTrips)^2) %>% 
  group_by() %>% 
  summarise(RMSE = (sum(Difference)/nrow(Data_Staff_Weekly_Test))^0.5)








#- Random visualisation ----
# Data_Student_Weekly %>% 
#   group_by(School) %>% 
#   summarise(`Mean trips` = mean(WeeklyTrips)) %>% 
#   ggplot()+
#   geom_col(aes(x=School, y=`Mean trips`)) +
#   geom_text(aes(x=School, y=`Mean trips`,label=round(`Mean trips`, digits=2)), vjust=1.6, color="white",
#             position = position_dodge(0.5), size=3.5) +
#   ggtitle("Student weekly mean (trips) by school - ADFA sample")
# 
# Days_by_School <- Data_Student %>% 
#   group_by( Day, School) %>% 
#   summarise(trips = n()) %>% 
#   group_by( Day, School) %>% 
#   summarise(`Trips` = sum(trips))
# 
# Days_by_School$Day <- factor(Days_by_School$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) 
#   
# Days_by_School %>% 
#   ggplot() +
#   geom_col(aes(x=Day, y=`Trips`, fill=School)) +
#   facet_wrap(~School) +
#   ggtitle("School number of student trips by day - ADFA sample")