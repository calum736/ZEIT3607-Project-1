#---- Packages ----
library(tidyverse)

#---- Read data ----
directory <- getwd()
student_data <- read.csv(file = 'data/1_Students.csv')
staff_data <- read.csv(file = 'data/1_Staff.csv')

student_IDs <- read.csv(file = 'data/Group_3_StudentIDs.csv')
staff_IDs <- read.csv(file = 'data/Group_3_StaffIDs.csv')

#---- Pull sample subset ----
student_sample <- filter(student_data, student_data$ID %in% student_IDs$ID)
staff_sample <- filter(staff_data, staff_data$ID %in% staff_IDs$ID)

# Write samlple sets to CSV
write.csv(student_sample, "Group_3_Student_Data.csv")
write.csv(staff_sample, "Group_3_Staff_Data.csv")

student_sample %>%
  #filter(Day == "Wednesday") %>%
  ggplot() +
  geom_bar(aes(Entrance, fill = Day)) +
  facet_wrap("Day")



z1 <- student_sample %>% 
      filter(Day == "Wednesday") %>% 
      count(Entrance)
z2 <- student_sample %>% 
      filter(Day == "Wednesday") %>% 
      count(Exit)
#z3[ <- z1[z2, Entrance - i.Exit]
      
