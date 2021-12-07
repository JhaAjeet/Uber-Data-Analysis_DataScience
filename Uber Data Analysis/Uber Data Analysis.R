library(ggplot2)
# This is the backbone of this project.
# It is the most popular data visualization library. 
# That is most widely used for creating aesthetic visualization plots.

library(ggthemes)
# This is more of an add-on to our main ggplot2 library.
# we can create better extra themes and scales with the mainstream ggplot2 package.

library(lubridate)
# Our dataset involves various time-frames.
# In order to understand our data in separate time categories,
# we will make use of the lubridate package.

library(dplyr)
# This package is the lingua franca of data manipulation in R.

library(tidyr)
# This package will help you to tidy your data. 
# The basic principle of tidyr is to tidy the columns where each variable is present in a column,
# each observation is represented by a row and each value depicts a cell.

library(DT)
# Datatables.

library(scales)
# With the help of graphical scales.

knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)

## Creating vector of colors to be implemented in our plots

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

## Reading the Data into their designated variables
# Now read several csv file data from April 2014 to September 2014. 
# we will perform the appropriate formatting of Date.Time column.
# we will proceed to create factors of time objects like day, month, year etc.

apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))


data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))


## Plotting the trips by the hours in a day
# we will use the ggplot function to plot the number of trips that the passengers had made in a day
# we can understand how the number of passengers fares throughout the day
#  We observe that the number of trips are higher in the evening around 5:00 and 6:00 PM

hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)


ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)



## Plotting data by trips during every day of the month
# we will learn how to plot our data based on every day of the month.
# We observe from the resulting visualization that 30th of the month had the highest trips in the year which is mostly contributed by the month of April.

day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)


ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)



## Number of Trips taking place during months in a year
#  we will visualize the number of trips that are taking place each month of the year
#  In the output visualization, we observe that most trips were made during the month of September
#  we also obtain visual reports of the number of trips that were made on every day of the week.


month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)



ggplot(month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)



## Finding out the number of Trips by bases
#  we plot the number of trips that have been taken by the passengers from each of the bases
#  There are five bases in all out of which we observe that B02617 had the highest number of trips
#  this base had the highest number of trips in the month B02617
#  Thursday observed highest trips in the three bases – B02598, B02617, B02682.


ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")


ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)


ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)



## Creating a Heatmap visualization of day, hour and month
#  we will learn how to plot heatmaps using ggplot().five heatmap plots 

# 1  First, we will plot Heatmap by Hour and Day.
# 2  Second, we will plot Heatmap by Month and Day.
# 3  Third, a Heatmap by Month and Day of the Week.
# 4  Fourth, a Heatmap that delineates Month and Bases.
# 5  Finally, we will plot the heatmap, by bases and day of the week.


day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)



ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")


ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")



ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")



month_base <-  data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n()) 

day0fweek_bases <-  data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n()) 

ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Bases")



ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")


## Creating a map visualization of rides in New York
#  In the final section, we will visualize the rides in New York city by creating a geo-plot that will help us to visualize the
#  rides during 2014 (Apr – Sep) and by the bases in the same period.


min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004


ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")


ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")

