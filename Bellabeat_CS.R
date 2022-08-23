library(ggplot2)
library(readr)
library(skimr)
library(tibble)
library(tidyverse)
library(dplyr)
library(lubridate)
library(here)
library(janitor)
library(openxlsx)

#analyze smart device usage data in order to gain insight into how consumers use non-Bellabeat smart
#devices

#Renamed each dataset for easier analysis

activity <- tibble(dailyActivity_merged)
calories <- tibble(dailyCalories_merged)
intensities <- tibble(hourlyIntensities_merged)
daily_steps <- tibble(dailySteps_merged)
sleep <- tibble(sleepDay_merged)
hourly_steps <- tibble(hourlySteps_merged)

View(activity)
View(calories)
View(intensities)
View(daily_steps)
View(sleep)
View(hourly_steps)

#looked for duplicates in data
sum(duplicated(activity))
sum(duplicated(sleep))
sum(duplicated(daily_steps))
sum(duplicated(hourly_steps))

#dropped all n/a values
activity <- activity %>% 
  distinct() %>% 
  drop_na()
hourly_steps <- hourly_steps %>% 
  distinct() %>% 
  drop_na()
sleep <- sleep %>% 
  distinct() %>% 
  drop_na()

#converted some variables to hours just in case 

#average = .35 hrs
very_active_hr <- activity$VeryActiveMinutes / 60
summary(very_active_hr)
#average = .22 hrs
fairly_active_hr <- activity$FairlyActiveMinutes / 60
summary(fairly_active_hr)
#average = 3.2 hrs
lightly_active_hr <- activity$LightlyActiveMinutes / 60
summary(lightly_active_hr)
#average = 6.9 hrs max = 13.26 hrs min = .96 hrs
sleep_hr <- sleep$TotalMinutesAsleep / 60
summary(sleep_hr)

#formatted all dates and time to be same

# intensities
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
# calories
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
# activity
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
# sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")
  
#discovered how many people in each table
n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(hourly_steps$Id)

#clean all names to make data easier to work with
clean_names(activity)
activity <- rename_with(activity, tolower)
clean_names(calories)
calories <- rename_with(calories, tolower)
clean_names(intensities)
intensities <- rename_with(intensities, tolower)
clean_names(sleep)
sleep <- rename_with(sleep, tolower)
clean_names(hourly_steps)
hourly_steps <- rename_with(hourly_steps, tolower)
clean_names(daily_steps)
daily_steps <- rename_with(daily_steps, tolower)

#noticed the date/time table was mislabeled
hourly_steps<- hourly_steps %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time,
                                format ="%m/%d/%Y %I:%M:%S %p" ,
                                tz=Sys.timezone()))

#brought the two datasets together to search for correlations 
activity_sleep_daily <- merge(activity, sleep,
                              by = c("id", "date"))
glimpse(activity_sleep_daily)

#no correlation found between sleep and total steps
garrange(
  ggplot(activity_sleep_daily, aes(x=totalsteps, y=totalminutesasleep))+
    geom_jitter() +
    geom_smooth(color = "pink") + 
    labs(title = "Minutes Asleep to Daily Steps", x = "Daily steps", y= "Minutes asleep") +
    theme(panel.background = element_blank(),
          plot.title = element_text( size=16)),
  
  #no correlation found between steps and calories
  ggplot(activity_sleep_daily, aes(x=totalsteps, y=calories))+
    geom_jitter() +
    geom_smooth(color = "pink") + 
    labs(title = "Daily Steps to Calories", x = "Daily steps", y= "Calories") +
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14))
)

#created a table that consists of averages from multiple datasets
daily_avg <- activity_sleep_daily %>% 
  group_by(id) %>% 
  summarize(avg_daily_steps = mean(totalsteps),
            avg_daily_calories = mean(calories),
            avg_daily_sleep = mean(totalminutesasleep))


#created a new variable that allows me to define each user by their level of activity
daily_avg <- daily_avg %>% 
  mutate(user_type = case_when(
  avg_daily_steps < 5000 ~ "sedentary",
  avg_daily_steps >= 5000 & avg_daily_steps < 7499 ~
  "lightly active",
  avg_daily_steps >= 7500 & avg_daily_steps < 9999 ~
  "fairly active",
   avg_daily_steps >= 10000 ~ "very active"))


write.xlsx(daily_avg, file = "daily_avgs.xlsx",
      sheetName = "daily_avgs", append = FALSE)

write.xlsx(hourly_steps, file = "hourly_steps.xlsx",
           sheetName = "hourly_steps", append = FALSE)

#separated date and time into two separate columns
hourly_steps$date <- as.Date(hourly_steps$date_time)
hourly_steps$time <- format(as.POSIXct(hourly_steps$date_time),
                            format = "%H:%M:%S")
#removed old column
hourly_steps <- hourly_steps[ , ! names(hourly_steps) %in% c("date_time", "data")]

#added more data to the table from another dataset for analysis
daily_avg <- merge(daily_avg, intensities, by = "id")


#more calories are burned through higher intensity not more steps? no correation found

#how many calories burned by each user type
daily_avg %>% 
  group_by(user_type) %>% 
  summarise(avg_daily_calories = sum(avg_daily_calories))


#percent of user types
user_type_percent <- user_type %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))


#created a pie chart to visualize different user types as a whole
user_type_percent$user_type <- factor(user_type_percent$user_type , levels = c("very active", "fairly active", "lightly active", "sedentary"))

user_type_percent %>%
  ggplot(aes(x="",y=total_percent, fill=user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = c("#85e085","#e6e600", "#ffd480", "#ff8080")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="User Types")

#display days of the week
activity$weekday <- weekdays(activity$activitydate)

hourly_steps2 <- hourly_steps$weekday <- weekdays(hourly_steps$date)

weekday_avg_steps <- activity %>% 
  group_by(weekday) %>% 
  summarize(weekday_avg_step = mean(totalsteps))



dailyIntensities_merged$weekday <- weekdays(dailyIntensities_merged$ActivityDay)

write.xlsx(hourly_steps, file = "hourlysteps.xlsx",
           sheetName = "hourlysteps", append = FALSE)



  