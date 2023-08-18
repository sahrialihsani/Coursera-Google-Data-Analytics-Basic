library(dplyr)
library(readr)
library(janitor)
library(lubridate)
data_all <- list.files(path = "D:/Course/Coursera/Google Data Analytics/Capstone/Capstone 1",  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data_all

as.data.frame(data_all)

#write.csv(data_all,"D:/Course/Coursera/Google Data Analytics/Capstone/data_combine.csv")

#remove dupplicate data
print(nrow(data_all))
handle_duplicate = data_all %>% distinct()
print(nrow(handle_duplicate))
#no duplicate data

#remove blank in rows
remove_blank <- handle_duplicate %>% remove_empty("rows")
print(nrow(remove_blank))
#no blank values in rows

#trim
remove_trim <- remove_blank %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

print(nrow(remove_trim))

glimpse(remove_trim)

#Process

#create column ride_length 
new_data <- remove_trim %>% 
  mutate(ride_length = ended_at-started_at)

glimpse(new_data)
#format the ride_length
new_data$ride_length <- hms::hms(seconds_to_period(new_data$ride_length))
glimpse(new_data)

#create column day_of_week
new_data$day_of_week <- weekdays(new_data$started_at)

new_data$month <- format(new_data$started_at, "%m")
unique(new_data$month)
new_data <-new_data %>% mutate(month= case_when(month == "01" ~ "January",
                                                month == "02" ~ "February",
                                                month == "03" ~ "March",
                                                month == "04" ~ "April",
                                                month == "05" ~ "May",
                                                month == "06" ~ "June",
                                                month == "07" ~ "July",
                                                month == "08" ~ "August",
                                                month == "09" ~ "September",
                                                month == "10" ~ "October",
                                                month == "11" ~ "November",
                                                month == "12" ~ "December",
                                                        TRUE ~ month))

# Map the weekdays to numbers (1 = Sunday, 7 = Saturday)
new_data$day_of_week_numeric <- match(new_data$day_of_week, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

glimpse(new_data)

#summary data
summary(new_data)

#more deeply
#install.packages('pastecs')
library(pastecs)
stat.desc(new_data)

#forgot to replace rideable type
fix_data <-new_data %>% mutate(rideable_type= case_when(rideable_type == "electric_bike" ~ "Electric Bike",
                                                        rideable_type == "classic_bike" ~ "Classic Bike",
                                                        rideable_type == "docked_bike" ~ "Docked Bike",
                                                        TRUE ~ rideable_type))
unique(fix_data$rideable_type)
library(hms)
#cyclistic_used$ride_length<-trimws(cyclistic_used$ride_length)
#glimpse(cyclistic_used)

#cyclistic_used$ride_length <- as_hms(cyclistic_used$ride_length)
#glimpse(cyclistic_used)

# Extract seconds
fix_data <- fix_data %>% mutate(ride_length_second=as.numeric(ride_length)) 
glimpse(fix_data)
fix_data$member_casual <- as.factor(fix_data$member_casual)
fix_data$rideable_type <- as.factor(fix_data$rideable_type)
fix_data$day_of_week <- as.factor(fix_data$day_of_week)
fix_data$month <- as.factor(fix_data$month)
glimpse(fix_data)


#Analyze
write.csv(fix_data,"D:/Course/Coursera/Google Data Analytics/Capstone/data_cleaned.csv")

#clean_data <- read.csv('D:/Course/Coursera/Google Data Analytics/Capstone/data_cleaned.csv')
#unique(clean_data$rideable_type)

#Question 1 - How do annual member & casual riders use cyclistic bikes differently
cyclistic_used <- fix_data %>% select(member_casual,rideable_type, month, day_of_week, ride_length, ride_length_second)
glimpse(cyclistic_used)

write.csv(cyclistic_used,"D:/Course/Coursera/Google Data Analytics/Capstone/data_used.csv")

#mean ride in minutes
mean_ride_by_member_and_bike <- cyclistic_used %>% group_by(member_casual, rideable_type) %>%
  summarize(mean_ride = mean(ride_length_second, na.rm = TRUE)/60)

#max ride in minutes
max_ride_by_member_and_bike <- cyclistic_used %>% group_by(member_casual, rideable_type) %>%
  summarize(max_ride = max(ride_length_second, na.rm = TRUE)/60)

#Average of ride length for each month (in minute)
mean_ride_by_member_and_month <- cyclistic_used %>% group_by(member_casual, month) %>%
  summarize(mean_ride = mean(ride_length_second, na.rm = TRUE)/60)

#Average of ride length per day (in minute)
mean_ride_by_member_and_day <- cyclistic_used %>% group_by(member_casual, day_of_week) %>%
  summarize(mean_ride = mean(ride_length_second, na.rm = TRUE)/60)

#Average of bike used per month
mean_ride_by_bike_and_month <- cyclistic_used %>% group_by(rideable_type, month) %>%
  summarize(mean_ride = mean(ride_length_second, na.rm = TRUE)/60)

#Average of bike used per day
mean_ride_by_bike_and_day <- cyclistic_used %>% group_by(rideable_type, day_of_week) %>%
  summarize(mean_ride = mean(ride_length_second, na.rm = TRUE)/60)

#Average of bike used per month (for each member type)
mean_ride_by_member_bike_and_month <- cyclistic_used %>% group_by(member_casual,rideable_type, month) %>%
  summarize(mean_ride = mean(ride_length_second, na.rm = TRUE)/60)

#Average of bike used per day (for each member type)
mean_ride_by_member_bike_and_day <- cyclistic_used %>% group_by(member_casual,rideable_type, day_of_week) %>%
  summarize(mean_ride = mean(ride_length_second, na.rm = TRUE)/60)

library(tidyverse)
ggplot(data = cyclistic_used, aes(member_casual,ride_length_second)) + geom_col()




