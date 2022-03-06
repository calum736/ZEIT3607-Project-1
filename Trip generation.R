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

# Blank df to tally a cumulative count of arrivals and departures
ts_count <- read.csv(file = 'data/ts_count_template.csv')

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
#student_sample %>%
  ##filter(Day == "Wednesday") %>%
  #ggplot() +
  #geom_bar(aes(Entrance, fill = Day)) +
  #facet_wrap("Day")

#AIM 
#Need to predict trip data based on adfa campus data and above parameters

#TODO
#Flow in versus flow out - plot cumulative curve to find periods when most busy.

arrivals <- student_sample %>% 
            group_by(Day, Entrance) %>% 
            summarise(n = n())

departures <- student_sample %>% 
              group_by(Day, Exit) %>% 
              summarise(n = n())

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

a <- student_sample %>% 
        group_by(Day, Entrance) %>% 
        summarise(n1 = n()) %>% 
        unite(day_time, Day:Entrance, remove = FALSE)
b <- student_sample %>%
        group_by(Day, Exit) %>% 
        summarise(n2 = n()) %>% 
        unite(day_time, Day:Exit, remove = FALSE)
c <- full_join(a, b, by="day_time")


#Analyse the adfa data, establish rates
#Apply rates and parameters to new campus population

