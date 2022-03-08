# UPPERCASE vars denote model parameters
# lowercase vars denote observations from adfa data

#---- PACKAGES ----
library(tidyverse)

#---- READ CSV DATA ----
student_data <- read.csv(file = 'data/1_Students.csv')
staff_data <- read.csv(file = 'data/1_Staff.csv')

student_IDs <- read.csv(file = 'data/Group_3_StudentIDs.csv')
staff_IDs <- read.csv(file = 'data/Group_3_StaffIDs.csv')

# Blank df to tally a cumulative count of arrivals and departures
ts_count <- read.csv(file = 'data/ts_count_template.csv')

trips_by_zone <- read.csv(file = 'data/Table 2 - Number of trips from the town centres to the ADFA Campus in 2020.csv')

#---- MODEL PARAMETERS ----
CAREER_BREAKDOWN <- c(
  "PGRD"=0.60,
  "UGRD"=0.40
)

GENDER <- c(
  "M"=0.50,
  "F"=0.50
)

# Job title expected composition as per brief
# Unsure, better to use this or use proportion of people by faculty???
JOB_BREAKDOWN <- c(
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

CONTRACT_HRS <- c(
  "Full-Time"=0.80,
  "Part-Time"=0.20
)

#Double check this one - brief was not clear 
FACULTY_BREAKDOWN <- c(
  "SEIT"=0.40,
  "HASS"=0.20,
  "BUS"=0.20,
  "SCI"=0.20
)

# Staff hrs schedule see page 51 of EA (too much detail, ignore for now)
# https://www.hr.unsw.edu.au/services/indrel/The%20University%20of%20New%20South%20Wales%20(Professional%20Staff)%20Enterprise%20Agreement%202018.pdf


N_STUDENTS <- 5000
N_STAFF <- 700

BASE_YEAR <- 2020
TARGET_YEAR <- 2040
TRIP_DURATION_GROWTH <- 0.15 # travel times expected to increase 15% in target year

#---- GET COUNTS ----
n_students <- n_distinct(student_data$ID)
n_staff <- n_distinct(staff_data$ID)

#---- EXTRACT GROUP 3 RECORDS ----
student_sample <- filter(student_data, student_data$ID %in% student_IDs$ID)
staff_sample <- filter(staff_data, staff_data$ID %in% staff_IDs$ID)

# Write sample set to CSV for future reference
write.csv(student_sample, "Group_3_Student_Data.csv")
write.csv(staff_sample, "Group_3_Staff_Data.csv")

# What can we learn about our sample???

n_sample_students = n_distinct(student_sample$ID)
n_sample_staff = n_distinct(staff_sample$ID)
  
#Proportion of PG : UG 
career_breakdown <- student_sample %>% 
                group_by(ID, Career) %>% 
                summarise() %>% 
                group_by(Career) %>% 
                summarise(n = n()) %>% 
                mutate(Freq = n/sum(n))

#Proportion of students and staff by faculty
faculty_breakdown <- bind_rows(
                student_sample %>% 
                group_by(ID, School) %>% 
                summarise()
                ,
                staff_sample %>% 
                group_by(ID, School) %>% 
                summarise() 
                ) %>%
                group_by(School) %>% 
                summarise(n = n()) %>% 
                mutate(Freq = n/sum(n))
       
#Proportion of staff by job title         
job_breakdown <- staff_sample %>% 
                group_by(ID, JobTitle) %>% 
                summarise() %>% 
                group_by(JobTitle) %>% 
                summarise(n = n()) %>% 
                mutate(Freq = n/sum(n))

#Proportion of trips by zone
trips_by_zone <- trips_by_zone %>%
                mutate(Freq = Weekly.Trips.to.ADFA.Campus/sum(Weekly.Trips.to.ADFA.Campus))

cross_prod_list <- function (A, B) {
  x_list <- vector(mode="numeric", len=0) #should mode be "list"? 
  for(i in 1:length(A)){
    for(j in 1:length(B)){
      key <- paste(names(A)[[i]], names(B)[[j]], sep="_")
      value <- A[[i]] * B[[j]]
      print(key)
      print(value)
      x_list[[key]] = value
    }
  }
  return(x_list)
}

#Average trips to campus per week per student by school and career (counts number of records)
#??? No post grad sci students in our sample
av_trips_student <- student_sample %>% 
  group_by(ID, School, Career) %>% 
  summarise(n = n()) %>% 
  #mutate(Freq = n/sum(n)) %>% 
  group_by(School, Career) %>% 
  summarise("Mean" = mean(n)) %>% 
  mutate(ID = paste(School, Career, sep="_"), .before=School)

SYNTH_STUDENT <- enframe((cross_prod_list(FACULTY_BREAKDOWN, CAREER_BREAKDOWN)), 
                         name="ID", 
                         value="Freq") %>% 
  mutate(n = Freq * N_STUDENTS) %>% 
  full_join(av_trips_student, by ="ID") %>% 
  mutate("n trips" = n * Mean) %>%  # this gives the number trips to campus per week (not per day)
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"TOTAL")))

av_trips_staff <- staff_sample %>% 
  group_by(ID, School, FulltimeParttime) %>% # job titles don't match ones given in brief, replace with level
  summarise(n = n()) %>% 
  #mutate(Freq = n/sum(n)) %>% 
  group_by(School, FulltimeParttime) %>% 
  summarise("Mean" = mean(n)) %>% 
  mutate(ID = paste(School, FulltimeParttime, sep="_"), .before=School)

SYNTH_STAFF <- enframe((cross_prod_list(FACULTY_BREAKDOWN, CONTRACT_HRS)), 
                         name="ID", 
                         value="Freq") %>% 
  mutate(n = Freq * N_STAFF) %>% 
  full_join(av_trips_staff, by ="ID") %>% 
  mutate("n trips" = n * Mean) %>%  # this gives the number trips to campus per week (not per day)
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"TOTAL")))






    
#Next part calculates number of people on campus at any given time from sample data
# Expensive op but does the job
count_entry_exit <- function(ts_count, sample_df) {
  for(i in 1:nrow(sample_df)) {
    # Look up and increment entrance time
    lookup <- paste(sample_df$Day[i], sample_df$Entrance[i], sep="_")
    r <- which(ts_count$ï..ID == lookup)
    if(length(r) > 0){
      ts_count$Entrance[r] = ts_count$Entrance[r] + 1
      #print("+1")
    }
    lookup <- paste(sample_df$Day[i], sample_df$Exit[i], sep="_")
    r <- which(ts_count$ï..ID == lookup)
    if(length(r) > 0){
      ts_count$Exit[r] = ts_count$Exit[r] + 1
      #print("-1")
    }
  }
  return(ts_count)
}
  
# ts_count <- count_entry_exit(ts_count, student_data)
# ts_count <- count_entry_exit(ts_count, staff_data)
ts_count <- count_entry_exit(ts_count, student_sample)
ts_count <- count_entry_exit(ts_count, staff_sample)

for(i in 1:nrow(ts_count)) {
  if(i == 1){
    ts_count$Count[i] = ts_count$Entrance[i] - ts_count$Exit[i]
  } else {
    ts_count$Count[i] = ts_count$Count[i-1] + ts_count$Entrance[i] - ts_count$Exit[i]
  }
}

write.csv(ts_count, "ts_count_output.csv")

ts_count %>%
  #filter(Day == "Sunday") %>%
  ggplot() +
  geom_line(aes(x=ï..ID,y=Count, group=1)) 
#facet_wrap("Day")

#Which arrival/departure period has greatest number people?
# sort ts_count by highest to lowest on entrance col to find busiest times



# arrivals <- student_sample %>% 
#   filter(School == "SEIT")
# ts_count <- count_entry_exit(ts_count, z)





