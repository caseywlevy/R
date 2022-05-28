# Project details at https://www.kaggle.com/code/caseylevy/data-manipulation-using-r


# Installing necessary packages

install.packages('tidyverse')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('janitor')

# Loading packages

library(tidyverse) # Used for data manipulation, exploration, visualization
library(lubridate) # Makes it easier to work with dates and times
library(ggplot2) # Used to create data visualizations
library(janitor) # Used to examine and clean dirty data

# Confirm that packages have successfully installed

installed.packages()

# Set working directory

setwd("~/R/CyclisticCaseStudy/2021/CsvFiles")

# Show current working directory

getwd()

# Upload data sets

Jan21 <- read_csv("202101-divvy-tripdata.csv")
Feb21 <- read_csv("202102-divvy-tripdata.csv")
Mar21 <- read_csv("202103-divvy-tripdata.csv")
Apr21 <- read_csv("202104-divvy-tripdata.csv")
May21 <- read_csv("202105-divvy-tripdata.csv")
Jun21 <- read_csv("202106-divvy-tripdata.csv")
Jul21 <- read_csv("202107-divvy-tripdata.csv")
Aug21 <- read_csv("202108-divvy-tripdata.csv")
Sep21 <- read_csv("202109-divvy-tripdata.csv")
Oct21 <- read_csv("202110-divvy-tripdata.csv")
Nov21 <- read_csv("202111-divvy-tripdata.csv")
Dec21 <- read_csv("202112-divvy-tripdata.csv")

# Compare column names to ensure matching headers before combining

colnames(Jan21)
colnames(Feb21)
colnames(Mar21)
colnames(Apr21)
colnames(May21)
colnames(Jun21)
colnames(Jul21)
colnames(Aug21)
colnames(Sep21)
colnames(Oct21)
colnames(Nov21)
colnames(Dec21)

# Combine 12 csv files into one for easier reference

combined_rides <- rbind(Jan21,Feb21,Mar21,Apr21,May21,Jun21,Jul21,Aug21,Sep21,Oct21,Nov21,Dec21)

# Inspect new combined data frame

# List all column names to ensure there are still exactly 13 columns
colnames(combined_rides)

# Check how many rows are in data frame to ensure all were included
nrow(combined_rides)

# Check dimensions of the data frame, rows by columns
dim(combined_rides)

# View the first 6 rows of data frame
head(combined_rides)

# View summary of columns and data types
str(combined_rides)

# View statistical summary of data
summary(combined_rides)

# Check that only appropriate inputs are in columns that could have typos, etc

table(combined_rides$rideable_type)
table(combined_rides$start_station_name)
table(combined_rides$end_station_name)
table(combined_rides$member_casual)

# Add columns for date, month, day, year, day of week

combined_rides$date <- as.Date(combined_rides$started_at)
combined_rides$month <- format(as.Date(combined_rides$date), "%m")
combined_rides$day <- format(as.Date(combined_rides$date), "%d")
combined_rides$year <- format(as.Date(combined_rides$date), "%Y")
combined_rides$day_of_week <- format(as.Date(combined_rides$date), "%A")

# Add a column for ride length

combined_rides$ride_length <- difftime(combined_rides$ended_at,combined_rides$started_at)

# Check dimensions of the data frame with added columns
dim(combined_rides)

# Convert ride_length to numeric to allow for further analysis

combined_rides$ride_length <- as.numeric(as.character(combined_rides$ride_length))

# Remove bad data, data includes entries from when bikes were taken out of docks and checked for quality by Cyclistic or ride_length was negative
# Create a new version of the dataframe (v2) since data is being removed
# Check number of rows before and after using dim function

dim(combined_rides)
combined_rides_v2 <- combined_rides[!(combined_rides$start_station_name == "HQ QR"|combined_rides$ride_length<0),]
dim(combined_rides_v2)

# Check for blank columns and blank rows
# Check number of rows before and after using dim function

dim(combined_rides_v2)
combined_rides_v2 <- janitor::remove_empty(combined_rides_v2, which = c("cols"))
combined_rides_v2 <- janitor::remove_empty(combined_rides_v2, which = c("rows"))
dim(combined_rides_v2)

# Descriptive analysis

# Descriptive analysis on ride_length, excluding NA with na.rm = TRUE
# All figures in seconds

mean(combined_rides_v2$ride_length,na.rm = TRUE) # average (total ride length / rides)
median(combined_rides_v2$ride_length,na.rm = TRUE) # midpoint number
max(combined_rides_v2$ride_length,na.rm = TRUE) # longest ride
min(combined_rides_v2$ride_length,na.rm = TRUE) # shortest ride

# Or use summary function to condense to one line

summary(combined_rides_v2$ride_length)

# Compare members and casual users using aggregate function

aggregate(combined_rides_v2$ride_length ~ combined_rides_v2$member_casual,FUN = mean)
aggregate(combined_rides_v2$ride_length ~ combined_rides_v2$member_casual,FUN = median)
aggregate(combined_rides_v2$ride_length ~ combined_rides_v2$member_casual,FUN = max)
aggregate(combined_rides_v2$ride_length ~ combined_rides_v2$member_casual,FUN = min)

# Compare by day of week
# Sort day of week in desired order and then aggregate

combined_rides_v2$day_of_week <- ordered(combined_rides_v2$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

aggregate(combined_rides_v2$ride_length ~ combined_rides_v2$member_casual + combined_rides_v2$day_of_week,FUN = mean)

# Analyze ridership data by type and weekday

combined_rides_v2 %>%
  mutate(weekday = wday(started_at,label = TRUE)) %>% #creates a new "weekday" column using wday() function, TRUE will return day in "Mon" format
  group_by(member_casual,weekday) %>% # groups by member_casual and weekday
  summarize(number_of_rides = n(),average_duration = mean(ride_length)) %>% #calculates the average duration
  arrange(member_casual, weekday) #sorts by member_casual and weekday in ascending order

# Visualize the number of rides by rider type

combined_rides_v2 %>%
  mutate(weekday = wday(started_at,label = TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarize(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=weekday, y=number_of_rides,fill=member_casual))+
  geom_col(position="dodge")
  
# Visualize by average duration

combined_rides_v2 %>%
  mutate(weekday = wday(started_at,label = TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarize(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=weekday, y=average_duration,fill=member_casual))+
  geom_col(position="dodge")

# Export

# Create a csv file of high level summary

counts <- aggregate(combined_rides_v2$ride_length~combined_rides_v2$member_casual+combined_rides_v2$day_of_week, FUN = mean)

write.csv(counts,file = "C:\\Users\\Casey\\Documents\\R\\CyclisticCaseStudy\\2021\\cyclisticdf.csv")

#Create a csv file of combined final data frame

write.csv(combined_rides_v2,file = "C:\\Users\\Casey\\Documents\\R\\CyclisticCaseStudy\\2021\\cyclisticcombined.csv")
