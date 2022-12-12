# Google-Data-Analytics-Casestudy---Bellabeat
## Introduction
#### About The Bellabeat
Urška Sršen and Sando Mur founded Bellabeat, a high-tech company that manufactures health-focused smart products. Since it was founded in 2013, Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women.


### Products:
  Bellabeat app: provides users with health data related to their activity, sleep, stress, menstrual cycle, and mindfulness habits.

*Leaf: connects to the Bellabeat app to track activity, sleep, and stress.

*Time: wellness watch - track user activity, sleep, and stress - connects to the Bellabeat app to give insights into daily wellness.

*Spring: a water bottle that tracks daily water intake - connects to the Bellabeat app to track hydration levels.

*Bellabeat membership: Subscription-based membership program, gives users 24/7 access to fully personalized guidance on nutrition, activity, sleep, health and beauty, and mindfulness based on their lifestyle and goals.

### 1.Ask Phase:
  A clear statement of the business task: “Identifying how customers use the company’s products in order to formulate the company’s marketing plan as effectively as possible.”

Questions for Analysis
What are some trends in smart device usage? How could these trends apply to Bellabeat customers? How could these trends help influence Bellabeat marketing strategy?
  
### 2.Prepare Phase:
  FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through [https://www.kaggle.com/datasets/arashnic/fitbit]): This Kaggle data set contains personal fitness tracker from thirty fitbit users. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes information about daily activity, steps, and heart rate that can be used to explore users’ habits.

### 3.Process Phase

### Load necessary libraries

library('readr')
library('tidyverse')
library('dplyr') 
library('janitor')
library('magrittr')
library('data.table')
library('lubridate')
library('ggplot2')
library('patchwork')

### Loading Datasets
*if this dataset doesnt load then download dataset and change the path in read.csv. then check the below codes*
  
activity <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
calories <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
intensities <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
steps <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
sleep <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/SleepDay_merged.csv")
weight <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/WeightLogInfo_merged.csv")
heartrate <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

  

### Activity
#### Data Checking 
activity$ActivityDate = as.POSIXct(activity$ActivityDate, format = '%m/%d/%y')
activity <- activity %>% rename(Date = ActivityDate) %>% 
  select(Id, Date, TotalSteps, TotalDistance, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes  
         ,SedentaryMinutes, Calories)
head(activity)

str(activity)

### calories
#### Data Reformatting
calories$ActivityDay = as.POSIXct(calories$ActivityDay, format = '%m/%d/%y')
calories <- calories %>% rename(Date = ActivityDay)
head(calories)

### sleepDay
#### Data Reformatting
sleep$SleepDay = as.POSIXct(sleep$SleepDay, format = '%m/%d/%y')
sleep <- sleep %>% rename(Date = SleepDay) %>%
  mutate(TotalHourAsleep = round(TotalMinutesAsleep/60)) %>%
  select(Id, Date, TotalHourAsleep)
head(sleep)

### sleepDay
#### Data Reformatting
sleep$SleepDay = as.POSIXct(sleep$SleepDay, format = '%m/%d/%y')
sleep <- sleep %>% rename(Date = SleepDay) %>%
  mutate(TotalHourAsleep = round(TotalMinutesAsleep/60)) %>%
  select(Id, Date, TotalHourAsleep)
head(sleep)

### weight
#### Data Reformatting
weight$Date = as.POSIXct(weight$Date, format = '%m/%d/%y')
weight <- weight %>% select(Id, Date, WeightKg, BMI)
head(weight)
weight %>% summarise(average_BMI = mean(BMI))

### heartrate

heartrate <-  setDT(heartrate)[, mean(Value), by = Id]
head(heartrate)
View(heartrate)


activity %>% select(TotalSteps, TotalDistance, Calories) %>% summary()

### 4.Analyze phase
### Plot:Total steps vs Calories 
This plot define the amount of calories loses accrding to ther step counts.

ggplot(data = activity) + geom_smooth(map = aes(x = TotalSteps, y = Calories)) + 
  geom_point(map = aes(x = TotalSteps, y = Calories)) +
  labs(title = 'Total Steps vs Calories')

### Total Distance vs Calories
This plot defines the calories loses based on distance covered.

ggplot(data = activity) + geom_smooth(map = aes(x = TotalDistance, y = Calories)) + 
  geom_point(map = aes(x = TotalDistance, y = Calories)) +
  labs(title = 'Total Distance vs Calories')

### Bar chart - TotalSleep for individual 
This bar chart shows the sleep hour for individual

sleep %>% select(TotalHourAsleep) %>% summary()

ggplot(data = sleep) + geom_bar(map = aes(x = TotalHourAsleep), fill = 'PURPLE')


intensities %>% select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>% summary()


### Plots: For Different Active Minutes
These plots show users different active timings

P1<- ggplot(data = activity) + geom_histogram(map = aes(x = VeryActiveMinutes), fill = 'blue') +
  labs(title = 'Histogram-VeryActiveMinutes')

P2<- ggplot(data = activity) + geom_histogram(map = aes(x = FairlyActiveMinutes), fill = 'red') +
  labs(title = 'Histogram-FairlyActiveMinutes')

P3<- ggplot(data = activity) + geom_histogram(map = aes(x = LightlyActiveMinutes), fill = 'green') +
  labs(title = 'Histogram-LightlyActiveMinutes')

P4<- ggplot(data = activity) + geom_histogram(map = aes(x = SedentaryMinutes), fill = 'purple') +
  labs(title = 'Histogram-SedentaryMinutes')

### Plot: Heartrate
This plot shows the average heartbeat af users but this data contains only 14 members so this plot shows the average heart beat of 14 members.

ggplot(heartrate, aes(x = Id,y = V1), group_by(Id)) + geom_line(aes(colour=V1))

 

### 5.Share
After sorting, cleaning and analyzing the datasets several conclusions can be drawn.

1.Most of the Fitbit users lead sedentary lifestyle.

2.The Steps Vs Calories plot shows that most of the users are in 10000 steps range

3.In Distance Plot, it shows approx 60 percent covers less distance.(below 10)

4.The bar chart about sleep shows a healthy trend of sleep time around 7 hours.

5.The users are in the range of sendantry and less active is absorbed from different active minutes plots.

6.The plot about heartrate shows a line that gives the average heart rate of users show they are in healthy condition.

These observations are based on limited data. maybe change when a different groups added together.


### 6.Act
Based on the observations,The app can give personal plans to work,diet & maintain their physique. 
And also can motivate the users to reach the goals set by the products and compliment them with rewards. this gives back repeated customers.
