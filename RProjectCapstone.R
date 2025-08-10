library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(purrr)
library(readr)
library(stringr)
library(tibble)

setwd("\Data Analytics Path to Power May 2025")

#Data Import Steps and preview
library(readr)
activity <- read.csv("~/Data Analytics Path to Power May 2025/RubensCapstone7.9.25/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")
activity2 <- read.csv("~/Data Analytics Path to Power May 2025/RubensCapstone7.9.25/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged4.12.16-5.12.16.csv")

head(activity)
head(activity2)

n_distinct(activity$Id)
n_distinct(activity2$Id)

intensity <- read.csv("~/Data Analytics Path to Power May 2025/RubensCapstone7.9.25/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlyIntensities_merged.csv")
intensity2 <- read.csv("~/Data Analytics Path to Power May 2025/RubensCapstone7.9.25/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged4.12.16-5.12.16.csv")

head(intensity)
head(intensity2)

n_distinct(intensity$Id)
n_distinct(intensity2$Id)

calories <- read.csv("~/Data Analytics Path to Power May 2025/RubensCapstone7.9.25/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlyCalories_merged.csv")
calories2 <- read.csv("~/Data Analytics Path to Power May 2025/RubensCapstone7.9.25/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged4.12.16-5.12.16.csv")

head(calories)
head(calories2)

sleep <- read.csv("~/Data Analytics Path to Power May 2025/RubensCapstone7.9.25/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteSleep_merged.csv")
sleep2 <- read.csv("~/Data Analytics Path to Power May 2025/RubensCapstone7.9.25/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged4.12.16-5.12.16.csv")

head(sleep)
head(sleep2)

weight <- read.csv("~/Data Analytics Path to Power May 2025/RubensCapstone7.9.25/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/weightLogInfo_merged.csv")
weight2 <- read.csv("~/Data Analytics Path to Power May 2025/RubensCapstone7.9.25/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged4.12.16-5.12.16.csv")

head(weight)
head(weight2)

#quick calculations for overall review and grasp of data
n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensity$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)


# Summary calories
calories %>% 
  select(Calories) %>% 
  summary()
# Summary intensity
intensity2 %>%
select(SedentaryMinutes,LightlyActiveMinutes,FairlyActiveMinutes,VeryActiveMinutes)%>%
summary()
# Summary activity
activity %>%
select(TotalSteps,TotalDistance,VeryActiveDistance,ModeratelyActiveDistance) %>%
summary()
# Summary sleep
sleep2 %>%
select(TotalMinutesAsleep,TotalTimeInBed) %>%
summary()
# Summary weight
weight %>%
select(WeightKg,BMI) %>%
summary()


# scatter plot with trend line - ggplot
ggplot(data = activity, aes(x=TotalSteps,y=Calories))+ geom_point()+ geom_smooth()+ ggtitle("Total Steps Vs. Calories")



int_new <- read.csv("~/Data Analytics Path to Power May 2025/Kaggle/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")

# New Chart - Convert ActivityHour to datetime
int_new$ActivityHour <- mdy_hms(int_new$ActivityHour)

# Extract the hour of the day
int_new$Hour <- hour(int_new$ActivityHour)

# Group by hour and calculate the Avg of TotalIntensity
hourly_summary <- int_new %>%
  group_by(Hour) %>%
  summarise(TotalIntensity = mean(TotalIntensity, na.rm = TRUE))

# Create the bar graph using ggplot2
ggplot(hourly_summary, aes(x = factor(Hour), y = TotalIntensity)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(x = "Time", y = "Avg Total Intensity", title = "Avg Total Intensity by the Hour") +
  theme_minimal()


#We observe a peak in intensity between 1700-1900 hours 
#(5 pm-7 pm). Historical data shows that the busiest time at the 
#Gym is between 1400-1900 (4 pm-7 pm). We can infer that the peak
#in Avg intensity is due to people utilizing the time after work to be 
#active either at the gym or somewhere else. I recommend that Bellabeat 
#provides an option to notify its users to spend this time being active 
#(ex.. walk, run, or gym)





