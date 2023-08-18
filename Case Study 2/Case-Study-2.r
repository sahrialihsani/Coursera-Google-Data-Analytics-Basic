## About Company
# Urška Sršen and Sando Mur founded Bellabeat, a high-tech company that manufactures health-focused smart products.
# Sršen used her background as an artist to develop beautifully designed technology that informs and inspires women
# around the world. Collecting data on activity, sleep, stress, and reproductive health has allowed Bellabeat to empower
# women with knowledge about their own health and habits. Since it was founded in 2013, Bellabeat has grown rapidly and
# quickly positioned itself as a tech-driven wellness company for women.
# By 2016, Bellabeat had opened offices around the world and launched multiple products. Bellabeat products became
# available through a growing number of online retailers in addition to their own e-commerce channel on their website. The
# company has invested in traditional advertising media, such as radio, out-of-home billboards, print, and television, but focuses
# on digital marketing extensively. Bellabeat invests year-round in Google Search, maintaining active Facebook and Instagram
# pages, and consistently engages consumers on Twitter. Additionally, Bellabeat runs video ads on Youtube and display ads on
# the Google Display Network to support campaigns around key marketing dates.
# Sršen knows that an analysis of Bellabeat’s available consumer data would reveal more opportunities for growth. She
# has asked the marketing analytics team to focus on a Bellabeat product and analyze smart device usage data in order to
# gain insight into how people are already using their smart devices. Then, using this information, she would like high-level
# recommendations for how these trends can inform Bellabeat marketing strategy.

## Character n Product
# ● Characters
# ○ Urška Sršen: Bellabeat’s cofounder and Chief Creative Officer
# ○ Sando Mur: Mathematician and Bellabeat’s cofounder; key member of the Bellabeat executive team
# ○ Bellabeat marketing analytics team: A team of data analysts responsible for collecting, analyzing, and
# reporting data that helps guide Bellabeat’s marketing strategy. You joined this team six months ago and have been
# busy learning about Bellabeat’’s mission and business goals — as well as how you, as a junior data analyst, can
# help Bellabeat achieve them.
# ● Products
# ○ Bellabeat app: The Bellabeat app provides users with health data related to their activity, sleep, stress,
# menstrual cycle, and mindfulness habits. 
# ○ Leaf: Bellabeat’s classic wellness tracker can be worn as a bracelet, necklace, or clip. The Leaf tracker connects
# to the Bellabeat app to track activity, sleep, and stress.
# ○ Time: This wellness watch combines the timeless look of a classic timepiece with smart technology to track user
# activity, sleep, and stress. 
# ○ Spring: This is a water bottle that tracks daily water intake using smart technology to ensure that you are
# appropriately hydrated throughout the day.
# ○ Bellabeat membership: Bellabeat also offers a subscription-based membership program for users.
# Membership gives users 24/7 access to fully personalized guidance on nutrition, activity, sleep, health
# and beauty, and mindfulness based on their lifestyle and goals.

##Ask
# 1. What are some trends in smart device usage?
# 2. How could these trends apply to Bellabeat customers?
# 3. How could these trends help influence Bellabeat marketing strategy?
#Business Task:
# 1. Identify potential opportunities for growth
# 2. Recommendations for the Bellabeat marketing strategy improvement based on trends in smart device usage.
#Data Integrity
# ROCCC
# 1. Reliability = LOW - only 30 participants data collected with a number of unknowns apparent such as age and gender.
# 
# 2. Originality = LOW - this data is Fitbit Fitness Tracker Data made available by Mobius stored on Kaggle, originally collected using Amazon Mechanical Turk.
# 
# 3. Comprehensive = MEDIUM - multiple fields and pieces of information are available but age and gender are not included and it is noted that only a totla of 2 logs of "Fat" and 67 of Weight are noted. There is also no mention of hydration logging in terms of Bellabeat's interest in marketability of their Spring product.
# 
# 4. Current = LOW - this data set is now 7 years old and there have been significant improvements and changes in habits within that time, especially during and after the pandemic.
# 
# 5. Cited = HIGH - the source is a highly data collector and the source is well documented.

##Prepare
# Define the dataset:
# FitBit Fitness Tracker Data in Kaggle
# 1. Daily Activity (Intensity, Steps, and Calories)
# 2. Hourly Calories
# 3. Daily sleep
# 4. Hourly steps
# 5. Weight

library(tidyverse)
library(hms)
library(shiny)
library(plotly)
library(skimr)

daily_activities <- read_csv('D:/Course/Coursera/Google Data Analytics/Capstone/Capstone 2/archive/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv')
daily_sleeps <- read_csv('D:/Course/Coursera/Google Data Analytics/Capstone/Capstone 2/archive/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv')
hourly_calories <- read_csv('D:/Course/Coursera/Google Data Analytics/Capstone/Capstone 2/archive/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv')
hourly_steps <- read_csv('D:/Course/Coursera/Google Data Analytics/Capstone/Capstone 2/archive/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv')
weight <- read_csv('D:/Course/Coursera/Google Data Analytics/Capstone/Capstone 2/archive/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv')

## Process
# inspect the data
glimpse(daily_activities)
glimpse(daily_sleeps)
glimpse(hourly_calories)
glimpse(hourly_steps)
glimpse(weight)

#check the duplicate data
sum(duplicated(daily_activities))
sum(duplicated(daily_sleeps)) #remove the duplicated data
sum(duplicated(hourly_calories))
sum(duplicated(hourly_steps))
sum(duplicated(weight))

#remove the duplicated data (daily sleep and minute sleep)
daily_sleeps <- daily_sleeps[-which(duplicated(daily_sleeps)), ]

#check the null values
sum(is.na(daily_activities))
sum(is.na(daily_sleeps))
sum(is.na(hourly_calories))
sum(is.na(hourly_steps))
sum(is.na(weight)) #removed column contain na

weight <- weight %>% select_if(~ !any(is.na(.)))
nrow(weight)
#Cleaned

#Remove whitespace
daily_activities <- daily_activities %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
daily_sleeps <- daily_sleeps %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
hourly_calories <- hourly_calories %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
hourly_steps <- hourly_steps %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
weight <- weight %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

#Cleaned

#Change date format 
glimpse(daily_activities)
daily_activities$ActivityDate <- as.Date(daily_activities$ActivityDate, format="%m/%d/%Y")
daily_activities <- daily_activities %>% 
  rename(Date = ActivityDate)
glimpse(daily_activities)

glimpse(daily_sleeps)
daily_sleeps$SleepDay <- as.POSIXct(daily_sleeps$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
daily_sleeps <- daily_sleeps %>% 
  rename(Date = SleepDay)
glimpse(daily_sleeps)

glimpse(hourly_calories)
hourly_calories$ActivityHour <- as.POSIXct(hourly_calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p",tz=Sys.timezone())
hourly_calories <- hourly_calories %>% 
  rename(Date = ActivityHour)
glimpse(hourly_calories)

glimpse(hourly_steps)
hourly_steps$ActivityHour <- as.POSIXct(hourly_steps$ActivityHour, format="%m/%d/%Y %I:%M:%S %p",tz=Sys.timezone())
hourly_steps <- hourly_steps %>% 
  rename(Date = ActivityHour)
glimpse(hourly_steps)

glimpse(weight)
weight$Date <- as.POSIXct(weight$Date, format="%m/%d/%Y")
glimpse(weight)

#Perform statistical summary
glimpse(daily_activities)
summary(daily_activities)

glimpse(daily_sleeps)
summary(daily_sleeps)

glimpse(hourly_calories)
summary(hourly_calories)

glimpse(hourly_steps)
summary(hourly_steps)

glimpse(weight)
summary(weight)

##Anaylsis
#relationship between calory dan step

#correlation among variable Step Total and Calories
cor(daily_activities$TotalSteps,daily_activities$Calories) #Berkorelasi positif

#Avg Calories and StepTotal by Id
cal_tot_id <-daily_activities %>% group_by(Id) %>%
  summarize(AvgStep = mean(TotalSteps), AvgCalory = mean(Calories))
tail(cal_tot_id)
#Avg Calories and StepTotal by Date
daily_activities %>% group_by(Date) %>%
  summarize(AvgStep = mean(TotalSteps), AvgCalory = mean(Calories))

#Avg Calories and StepTotal 
daily_activities %>%
  summarize(AvgStep = mean(TotalSteps), AvgCalory = mean(Calories))

#Sum, Average, Max Distance
daily_activities %>% group_by(Id) %>%
  summarize(sumDistance = sum(TotalDistance),avgDistance = mean(TotalDistance), maxDistance = max(TotalDistance))

#Sum, Average, Max Total Distance by Id and Date
daily_activities %>% group_by(Date) %>%
  summarize(sumDistance = sum(TotalDistance),avgDistance = mean(TotalDistance), maxDistance = max(TotalDistance))

#Sum, Average, Max Total Steps by Id and Date
total_steps <- daily_activities %>% group_by(Date) %>%
  summarize(sumSteps = sum(TotalSteps),avgSteps = mean(TotalSteps), maxSteps = max(TotalSteps))
head(total_steps)

#Sum, Average, Max Total Steps by Id
daily_activities$Id <- as.factor(daily_activities$Id)
# daily_activities <- daily_activities %>%
#   mutate(Id = paste0("C", as.numeric(Id)))

total_steps_person <- daily_activities %>% group_by(Id) %>%
  summarize(sumSteps = sum(TotalSteps),avgSteps = mean(TotalSteps), maxSteps = max(TotalSteps),sumCalories = sum(Calories),avgCalories = mean(Calories), maxCalories = max(Calories))
glimpse(total_steps_person)

#merge data hourly calories dan hourly step
merged_calories_steps_hourly <- merge(hourly_calories, hourly_steps, by = c("Id","Date"))
glimpse(merged_calories_steps_hourly)

#Extracted hour
merged_calories_steps_hourly <- merged_calories_steps_hourly %>%
  mutate(Hour = hour(merged_calories_steps_hourly$Date))

merged_calories_steps_hourly <-merged_calories_steps_hourly %>% mutate(Hour = case_when(Hour == 0 ~ 24,
                                                        TRUE ~ Hour))
glimpse(merged_calories_steps_hourly)

#correlation among variable Step Total and Calories (hourly)
cor(merged_calories_steps_hourly$StepTotal,merged_calories_steps_hourly$Calories) #Berkorelasi positif

#Average Calories and StepTotal By Id
merged_calories_steps_hourly %>% group_by(Id) %>%
  summarize(AvgStep = mean(StepTotal), AvgCalory = mean(Calories))

#Average Calories and StepTotal By Hour
hourly_step <- merged_calories_steps_hourly %>% group_by(Hour) %>%
  summarize(AvgStep = mean(StepTotal), AvgCalory = mean(Calories))
mean(hourly_step$AvgStep)
mean(hourly_step$AvgCalory)
#Average Calories and StepTotal By Date and Hour
avg_cal_step_hour <- merged_calories_steps_hourly %>% group_by(Date, Hour) %>%
  summarize(AvgStep = mean(StepTotal), AvgCalory = mean(Calories))
avg_cal_step_hour

#Average Calories and StepTotal 
merged_calories_steps_hourly %>%
  summarize(AvgStep = mean(StepTotal), AvgCalory = mean(Calories))


#Relationship between BMI and Total Step
merged_weight_activity <- merge(daily_activities, weight, by=c("Id","Date"))
merged_weight_activity <- merged_weight_activity %>%
  select(Id, Date, TotalSteps, TotalDistance, WeightKg, BMI, Calories)
glimpse(merged_weight_activity)

#correlation among variable BMI and Weight
cor(merged_weight_activity$WeightKg,merged_weight_activity$BMI) #Berkorelasi positif

#avg BMI and Weight
merged_weight_activity %>%
  summarize(avgBMI = mean(BMI),avgWeight = mean(WeightKg))

#avg BMI and Weight by Id
merged_weight_activity %>% group_by(Id) %>%
  summarize(avgBMI = mean(BMI),avgWeight = mean(WeightKg))

##activity with weight
merged_activity_weight <- merge(daily_activities, weight, by = c("Id","Date"))
glimpse(merged_activity_weight)

mean(merged_activity_weight$WeightKg)
max(merged_activity_weight$WeightKg)

mean(merged_activity_weight$BMI)

#correlation among variable Sedentary Minutes and Weight
cor(merged_activity_weight$SedentaryMinutes,merged_activity_weight$WeightKg) #Berkorelasi positif

#avg BMI and Weight
merged_activity_weight %>%
  summarize(avgSedentaryMinutes = mean(SedentaryMinutes),avgWeight = mean(WeightKg))

nrow(merged_activity_weight)

#avg BMI and Weight by Id
sed_weight<-merged_activity_weight %>% group_by(Date) %>%
  summarize(avgSedentaryMinutes = mean(SedentaryMinutes),avgWeight = mean(WeightKg))

merged_activity_weight$Id <- as.factor(merged_activity_weight$Id)
# daily_activities <- daily_activities %>%
#   mutate(Id = paste0("C", as.numeric(Id)))

total_sedentary_weight_person <- merged_activity_weight %>% group_by(Id) %>%
  summarize(sumWeight = sum(WeightKg),avgWeight = mean(WeightKg), maxWeight = max(WeightKg),sumSedentary = sum(SedentaryMinutes),avgSedentary = mean(SedentaryMinutes), maxSedentary = max(SedentaryMinutes))
total_sedentary_weight_person

total_sedentary_weight <- merged_activity_weight %>% group_by(Date) %>%
  summarize(sumWeight = sum(WeightKg),avgWeight = mean(WeightKg), maxWeight = max(WeightKg),sumSedentary = sum(SedentaryMinutes),avgSedentary = mean(SedentaryMinutes), maxSedentary = max(SedentaryMinutes))
total_sedentary_weight

##Visualize
#Relationship between Total Steps and Burned Calories
glimpse(daily_activities)
ggplot(data = daily_activities, aes(x = TotalSteps, y= Calories)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "tomato") +
  labs(title = 'Relationship between Total Steps and Burned Calories',
       x = 'Total Steps',
       y = 'Calories Burned') +
  theme_bw() +
  annotate("text", x = 25000, y = 1000, label = "Data are positively correlated ", color = "green", size=4)

#Relationship between Total Steps and Burned Calories (Hourly)
glimpse(merged_calories_steps_hourly)
ggplot(data = merged_calories_steps_hourly, aes(x = StepTotal, y= Calories )) + 
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "tomato") +
  labs(title = 'Relationship between Total Steps and Burned Calories (Hour)',
       x = 'Total Steps',
       y = 'Calories Burned') +
  theme_bw() +
  annotate("text", x = 8000, y = 200, label = "Data are positively correlated ", color = "green", size=4)

#Total Steps
glimpse(total_steps)
ggplot(data = total_steps, aes(x = Date, y= avgSteps)) + 
  geom_line(color = "blue") +
  geom_text(aes(label = round(avgSteps)), vjust = -0.5, color = "black", size = 2)+
  geom_point(color="gray")+
  labs(title = 'Average of Total Steps over 30 Days',
       x = 'Date',
       y = 'Total Steps') +
  theme_bw() +
  annotate("text", x = as.Date("2016-04-25"), y = 6000, label = "Fluctuated over week 1- week 3", color = "red", size = 4)+
  annotate("text", x = as.Date("2016-05-07"), y = 4000, label = "Last 7 days total steps decreased", color = "red", size = 4)

ggplot(data = hourly_step, aes(x = Hour, y= AvgStep)) + 
  geom_line(color = "blue") +
  geom_text(aes(label = round(AvgStep)), vjust = -0.5, color = "black", size = 2)+
  geom_point(color="gray")+
  labs(title = 'Hourly Total Steps',
       x = 'Hour',
       y = 'Total Steps') +
  annotate("text", x = 14, y = 300, label = "Active participants from 5am-7pm", color = "green", size = 4)+
  theme_bw() 
  

#Relationship between BMI and Weight
glimpse(merged_weight_activity)
ggplot(data = merged_weight_activity, aes(x = WeightKg, y = BMI)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "tomato") +
  labs(title = 'Relationship between Weight and BMI',
       x = 'Weight',
       y = 'BMI') +
  theme_bw() +
  annotate("text", x = 100, y = 25, label = "Data are positively correlated ", color = "green", size=4)

#Person Step and Calories
glimpse(total_steps_person)

new_labels <- c(
  "C1", "C2", "C3", "C4", "C5", 
  "C6", "C7", "C8", "C9", "C10",
  "C11", "C12", "C13", "C14", "C15", 
  "C16", "C17", "C18", "C19", "C20",
  "C21", "C22", "C23", "C24", "C25", 
  "C26", "C27", "C28", "C29", "C30",
  "C31", "C32", "C33")

ggplot(total_steps_person, aes(x = Id)) +
  geom_bar(aes(y = avgSteps), stat = "identity", fill = "blue") +
  geom_bar(aes(y = avgCalories), stat = "identity", fill = "red") +
  labs(title = "Total Steps and Calories for Each Person",
       x = "Customer ID",
       y = "Total Steps and Calories",
       fill = "Metrics") +  # Annotate the legend title
  scale_x_discrete(labels = new_labels) +  # Apply custom labels to x-axis
  theme_bw() +
  annotate("text", x = "2022484408", y = 15000, label = "Blue color shows Total Steps,", color = "blue", size=4) +
  annotate("text", x = "4020332650", y = 15000, label = "Red color shows calories", color = "red", size=4)

#Relationship between Sedentary Minutes and Weight
glimpse(merged_activity_weight)
ggplot(data = merged_activity_weight, aes(x = SedentaryMinutes, y= WeightKg )) + 
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "tomato") +
  labs(title = 'Relationship between Sedentary Minutes and Weight',
       x = 'Sedentary Minutes',
       y = 'Weight (Kg)') +
  theme_bw() +
  annotate("text", x = 1200, y = 50, label = "Data are positively correlated ", color = "green", size=4)

ggplot(total_sedentary_weight_person, aes(x = Id)) +
  geom_bar(aes(y = avgWeight), stat = "identity", fill = "blue") +
  labs(title = "Total Weight for Each Person",
       x = "Customer ID",
       y = "Weight",
       fill = "Metrics") +  # Annotate the legend title
  theme_bw()
