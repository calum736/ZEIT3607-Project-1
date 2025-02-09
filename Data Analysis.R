# UPPERCASE vars denote model parameters
# lowercase vars denote observations from adfa data

#---- PACKAGES ----
library(tidyverse)

#---- READ CSV DATA ----
student_data <- read.csv(file = "data/1_Students.csv")
staff_data <- read.csv(file = "data/1_Staff.csv")

student_IDs <- read.csv(file = "data/Group_3_StudentIDs.csv")
staff_IDs <- read.csv(file = "data/Group_3_StaffIDs.csv")

# Blank df to tally a cumulative count of arrivals and departures
ts_count <- read.csv(file = "data/ts_count_template.csv")

trips_by_zone <- read.csv(file = "data/Table 2 - Number of trips from the town centres to the ADFA Campus in 2020.csv")
zone_population <- read.csv(file = "data/Table 3 - Population of the town centres, in the past, and its projection from the future.csv")
zone_distance_city <- read.csv(file = "data/Table 4 - Average distance from the town centres to ADFA Campus and City Campus.csv")
zone_travel_time_city <- read.csv(file = "data/Table 5 - Mean, minimum, and maximum values of travel time from the town centres to the City Campus Site.csv")


#---- MODEL PARAMETERS ----
CAREER_BREAKDOWN <- c(
  "PGRD" = 0.60,
  "UGRD" = 0.40
)

# GENDER <- c( 
#   "M" = 0.50,
#   "F" = 0.50
# )

# Job title expected composition as per brief
# Unsure, better to use this or use proportion of people by faculty???
JOB_BREAKDOWN <- c(
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

CONTRACT_HRS <- c(
  "Full-Time" = 0.80,
  "Part-Time" = 0.20
)

# Double check this one - brief was not clear
FACULTY_BREAKDOWN <- c(
  "SEIT" = 0.40,
  "HASS" = 0.20,
  "BUS" = 0.20,
  "SCI" = 0.20
)

# Staff hrs schedule see page 51 of EA (too much detail, ignore for now)
# https://www.hr.unsw.edu.au/services/indrel/The%20University%20of%20New%20South%20Wales%20(Professional%20Staff)%20Enterprise%20Agreement%202018.pdf


N_STUDENT <- 5000
N_STAFF <- 700

BASE_YEAR <- 2020
TARGET_YEAR <- 2040

TRIP_DURATION_GROWTH <- 0.15 # travel times expected to increase 15% in target year

PT_STAFF_COST = 3.22
PT_STUDENT_COST = 1.66
AUTO_KM_COST = 0.35
AUTO_PARKING_COST = 5 

####################
# 0. DATA ANALYSIS #
####################

#---- GET COUNTS ----
n_students <- n_distinct(student_data$ID)
n_staff <- n_distinct(staff_data$ID)

#---- EXTRACT GROUP 3 RECORDS ----
student_sample <- filter(student_data, student_data$ID %in% student_IDs$ID)
staff_sample <- filter(staff_data, staff_data$ID %in% staff_IDs$ID)

# Write sample set to CSV for future reference
write.csv(student_sample, "output data/Group_3_Student_Data.csv")
write.csv(staff_sample, "output data/Group_3_Staff_Data.csv")

# What can we learn about our sample???

n_sample_students <- n_distinct(student_sample$ID)
n_sample_staff <- n_distinct(staff_sample$ID)

# Proportion of PG : UG
career_breakdown <- student_sample %>%
  group_by(ID, Career) %>%
  summarise() %>%
  group_by(Career) %>%
  summarise(n = n()) %>%
  mutate(Freq = n / sum(n))

# Proportion of students and staff by faculty
faculty_breakdown <- bind_rows(
  student_sample %>%
    group_by(ID, School) %>%
    summarise(),
  staff_sample %>%
    group_by(ID, School) %>%
    summarise()
) %>%
  group_by(School) %>%
  summarise(n = n()) %>%
  mutate(Freq = n / sum(n))


# Proportion of staff by job title
job_breakdown <- staff_sample %>%
  group_by(ID, JobTitle) %>%
  summarise() %>%
  group_by(JobTitle) %>%
  summarise(n = n()) %>%
  mutate(Freq = n / sum(n))

cross_prod_list <- function(A, B) {
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



temp <- student_sample %>%
  group_by(ID, Career) %>%
  summarise(n = n()) %>%
  # mutate(Freq = n/sum(n)) %>%
  group_by(Career) %>%
  summarise("Weekly mean (trips)" = mean(n)) 
  #mutate(ID = paste(School, Career, sep = "_"), .before = School)

ggplot(data=temp, aes(x=Career, y=`Weekly mean (trips)`, fill=Career)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(`Weekly mean (trips)`, digits=2)), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  ggtitle("Student weekly mean (trips) by career - ADFA sample")

temp1 <- student_sample %>%
  group_by(ID, School) %>%
  summarise(n = n()) %>%
  # mutate(Freq = n/sum(n)) %>%
  group_by(School) %>%
  summarise("Weekly mean (trips)" = mean(n)) 
#mutate(ID = paste(School, Career, sep = "_"), .before = School)

ggplot(data=temp1, aes(x=School, y=`Weekly mean (trips)`, fill=School)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(`Weekly mean (trips)`, digits=2)), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  ggtitle("Student weekly mean (trips) by school - ADFA sample")

temp2 <- student_sample %>%
  group_by(ID, Career, School) %>%
  summarise(n = n()) %>%
  # mutate(Freq = n/sum(n)) %>%
  group_by(Career, School) %>%
  summarise("Weekly mean (trips)" = mean(n)) 
#mutate(ID = paste(School, Career, sep = "_"), .before = School)


ggplot(data=temp2, aes(x=School, y=`Weekly mean (trips)`, fill=Career)) +
  geom_bar(stat="identity", position="dodge", width=0.5) + 
  geom_text(aes(label=round(`Weekly mean (trips)`, digits=2)), vjust=1.6, color="white",
            position = position_dodge(0.5), size=3.5)+
  ggtitle("Student weekly mean (trips) by career - ADFA sample")

temp3 <- student_sample %>% 
  group_by(ID, AdmitTerm, School) %>%
  summarise(n = n()) %>%
  # mutate(Freq = n/sum(n)) %>%
  group_by(AdmitTerm, School) %>%
  summarise("Weekly mean (trips)" = mean(n)) %>% 
  filter(AdmitTerm != "Term 1 2019") %>% 
  filter(AdmitTerm != "Term 2 2019") %>% 
  filter(AdmitTerm != "Term 1 2020") %>% 
  filter(AdmitTerm != "Term 2 2020") %>% 
  filter(AdmitTerm != "Term 3 2019")
  #mutate(ID = paste(School, Career, sep = "_"), .before = School)

temp3[temp3 == 'Semester 2 2011'] <- 'a Semester 2 2011'
temp3[temp3 == 'Semester 1 2012'] <- 'b Semester 1 2012'
temp3[temp3 == 'Semester 2 2012'] <- 'c Semester 2 2012'
temp3[temp3 == 'Semester 1 2013'] <- 'd Semester 1 2013'
temp3[temp3 == 'Semester 2 2013'] <- 'e Semester 2 2013'
temp3[temp3 == 'Semester 1 2014'] <- 'f Semester 1 2014'
temp3[temp3 == 'Semester 2 2014'] <- 'g Semester 2 2014'
temp3[temp3 == 'Semester 1 2015'] <- 'h Semester 1 2015'
temp3[temp3 == 'Semester 2 2015'] <- 'i Semester 2 2015'
temp3[temp3 == 'Semester 1 2016'] <- 'j Semester 1 2016'
temp3[temp3 == 'Semester 2 2016'] <- 'k Semester 2 2016'
temp3[temp3 == 'Semester 1 2017'] <- 'l Semester 1 2017'
temp3[temp3 == 'Semester 2 2017'] <- 'm Semester 2 2017'
temp3[temp3 == 'Semester 1 2018'] <- 'n Semester 1 2018'
temp3[temp3 == 'Semester 2 2018'] <- 'o Semester 2 2018'
temp3[temp3 == 'Semester 1 Canberra 2019'] <- 'p Semester 1 2019'
temp3[temp3 == 'Semester 2 Canberra 2019'] <- 'q Semester 2 2019'
temp3[temp3 == 'Semester 1 Canberra 2020'] <- 'r Semester 1 2020'

write.csv(temp3, 'temp.csv')

temp3 <- temp3[order(temp3$AdmitTerm),]
ggplot(data=temp3, aes(x=AdmitTerm, y=`Weekly mean (trips)`, fill=AdmitTerm)) +
  geom_bar(stat="identity") + 
  scale_x_discrete(label = function(x) substring(x, 1, 1)) +
  geom_text(aes(label=round(`Weekly mean (trips)`, digits=2)), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  ggtitle("Student weekly mean (trips) by admission term - ADFA sample") +
  facet_grid(School ~.)


temp4 <- staff_sample %>%
  group_by(ID, School, FulltimeParttime) %>%
  summarise(n = n()) %>%
  # mutate(Freq = n/sum(n)) %>%
  group_by(School, FulltimeParttime) %>%
  summarise("Weekly mean (trips)" = mean(n)) 
#mutate(ID = paste(School, Career, sep = "_"), .before = School)


ggplot(data=temp4, aes(x=School, y=`Weekly mean (trips)`, fill=FulltimeParttime)) +
  geom_bar(stat="identity", position="dodge", width=0.5) + 
  geom_text(aes(label=round(`Weekly mean (trips)`, digits=2)), vjust=1.6, color="white",
            position = position_dodge(0.5), size=3.5)+
  ggtitle("Staff weekly mean (trips) by FT/PT hours - ADFA sample") 
  
temp5 <- staff_sample %>%
  group_by(ID, School, Level) %>%
  summarise(n = n()) %>%
  # mutate(Freq = n/sum(n)) %>%
  group_by(School, Level) %>%
  summarise("Weekly mean (trips)" = mean(n)) 
#mutate(ID = paste(School, Career, sep = "_"), .before = School)


ggplot(data=temp5, aes(x=Level, y=`Weekly mean (trips)`, fill=School)) +
  geom_bar(stat="identity", position="dodge", width=0.5) + 
  geom_text(aes(label=round(`Weekly mean (trips)`, digits=2)), vjust=1.6, color="black",
            position = position_dodge(0.5), size=3.5)+
  ggtitle("Staff weekly mean (trips) by job level - ADFA sample")




sData %>%
  #filter(Day == "Wednesday") %>%
  ggplot() +
  geom_bar(aes(Entrance, fill = Day)) +
  facet_wrap("Day")







######################
# 1. TRIP GENERATION #
######################

# Average trips to campus per week per student by school and career (counts number of records)
# ??? No post grad sci students in our sample
av_trips_student <- student_sample %>%
  group_by(ID, School, Career) %>%
  summarise(n = n()) %>%
  # mutate(Freq = n/sum(n)) %>%
  group_by(School, Career) %>%
  summarise("Mean" = mean(n)) %>%
  mutate(ID = paste(School, Career, sep = "_"), .before = School)

SYNTH_STUDENT <- enframe((cross_prod_list(FACULTY_BREAKDOWN, CAREER_BREAKDOWN)),
  name = "ID",
  value = "Freq"
) %>%
  mutate(n = Freq * N_STUDENT) %>%
  full_join(av_trips_student, by = "ID") %>%
   mutate("n trips" = n * Mean) # %>% # this gives the number trips to campus per week (not per day)
write.csv(SYNTH_STUDENT, "output data/SYNTH_STUDENT_TRIPS.csv")

av_trips_staff <- staff_sample %>%
  group_by(ID, School, FulltimeParttime) %>% # job titles don't match ones given in brief, replace with level
  summarise(n = n()) %>%
  # mutate(Freq = n/sum(n)) %>%
  group_by(School, FulltimeParttime) %>%
  summarise("Mean" = mean(n)) %>%
  mutate(ID = paste(School, FulltimeParttime, sep = "_"), .before = School)

SYNTH_STAFF <- enframe((cross_prod_list(FACULTY_BREAKDOWN, CONTRACT_HRS)),
  name = "ID",
  value = "Freq"
) %>%
  mutate(n = Freq * N_STAFF) %>%
  full_join(av_trips_staff, by = "ID") %>%
   mutate("n trips" = n * Mean) # %>% # this gives the number trips to campus per week (not per day)
write.csv(SYNTH_STAFF, "output data/SYNTH_STAFF_TRIPS.csv")

TOTAL_TRIPS = sum(SYNTH_STUDENT$`n trips`, na.rm=TRUE) + sum(SYNTH_STAFF$`n trips`, na.rm=TRUE) #remove NAs

########################
# 2. TRIP DISTRIBUTION #
########################

# Factor in growth for each zone and the proportion of weekly trips to predict number of trips
# Probably incorrect method to predict since have used table 2 from brief
TRIPS_BY_ZONE <- zone_population %>% 
  full_join(trips_by_zone, by="Zone") %>% 
  mutate('2016_trips' = TOTAL_TRIPS * X2016 / X2020 * (Weekly.Trips.to.ADFA.Campus / sum(Weekly.Trips.to.ADFA.Campus))) %>% 
  mutate('2020_trips' = TOTAL_TRIPS * X2020 / X2020 * (Weekly.Trips.to.ADFA.Campus / sum(Weekly.Trips.to.ADFA.Campus))) %>% 
  mutate('2040_trips' = TOTAL_TRIPS * X2040 / X2020 * (Weekly.Trips.to.ADFA.Campus / sum(Weekly.Trips.to.ADFA.Campus))) %>% 
  select (-c(Weekly.Trips.to.ADFA.Campus, X2016, X2020, X2040)) 
write.csv(TRIPS_BY_ZONE, "output data/TRIPS_BY_ZONE.csv")

##################
# 3. MODE CHOICE #
##################

# Require data from Tables 4, 5 
# This doesn't take into account the 15 percent increase in travel duration by 2040
# Some info about what U indicates - https://brilliant.org/wiki/utility-functions/
MODE_CHOICE <- full_join(zone_travel_time_city, zone_distance_city, by="Zone") %>% 
  mutate(Reliability = (Maximum - Minimum)/Mean) %>% 
  mutate(U = ifelse(Mode == "Auto", 
                    -0.25 * Mean - 0.11 * AUTO_KM_COST,
                    ifelse(Mode == "PT_Student",
                           -0.26 - 0.15 * Mean - 0.13 * PT_STUDENT_COST - 0.01 * Reliability,
                           ifelse(Mode == "PT_Staff",
                                  -0.26 - 0.15 * Mean - 0.13 * PT_STAFF_COST - 0.01 * Reliability,
                                  ifelse(Mode == "Active",
                                         0.1 - 0.67 * Distance.to.City.Campus..km.,
                                         NA))))) 
write.csv(MODE_CHOICE, "output data/MODE_CHOICE.csv")

###############################
# EXTRA SUFF - IGNORE FOR NOW #
###############################

# Next part calculates number of people on campus at any given time from sample data
# Expensive op but does the job
# count_entry_exit <- function(ts_count, sample_df) {
#   for (i in 1:nrow(sample_df)) {
#     # Look up and increment entrance time
#     lookup <- paste(sample_df$Day[i], sample_df$Entrance[i], sep = "_")
#     r <- which(ts_count$ï..ID == lookup)
#     if (length(r) > 0) {
#       ts_count$Entrance[r] <- ts_count$Entrance[r] + 1
#       # print("+1")
#     }
#     lookup <- paste(sample_df$Day[i], sample_df$Exit[i], sep = "_")
#     r <- which(ts_count$ï..ID == lookup)
#     if (length(r) > 0) {
#       ts_count$Exit[r] <- ts_count$Exit[r] + 1
#       # print("-1")
#     }
#   }
#   return(ts_count)
# }
# 
# # ts_count <- count_entry_exit(ts_count, student_data)
# # ts_count <- count_entry_exit(ts_count, staff_data)
# ts_count <- count_entry_exit(ts_count, student_sample)
# ts_count <- count_entry_exit(ts_count, staff_sample)
# 
# for (i in 1:nrow(ts_count)) {
#   if (i == 1) {
#     ts_count$Count[i] <- ts_count$Entrance[i] - ts_count$Exit[i]
#   } else {
#     ts_count$Count[i] <- ts_count$Count[i - 1] + ts_count$Entrance[i] - ts_count$Exit[i]
#   }
# }
# 
# write.csv(ts_count, "output data/ts_count_output.csv")

#ts_count %>%
  # filter(Day == "Sunday") %>%
#  ggplot() +
#  geom_line(aes(x = ï..ID, y = Count, group = 1))
# facet_wrap("Day")

# Which arrival/departure period has greatest number people?
# sort ts_count by highest to lowest on entrance col to find busiest times



# arrivals <- student_sample %>%
#   filter(School == "SEIT")
# ts_count <- count_entry_exit(ts_count, z)
