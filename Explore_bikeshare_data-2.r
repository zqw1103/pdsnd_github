ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

What's the common month?

library(readr);
library(tidyr);
library(plyr)
library(dplyr);
library(ggplot2)

chi <- read.csv('chicago.csv')
ny <- read.csv('new_york_city.csv')
wash <- read.csv('washington.csv')

chi$Start.Month <- format(as.Date(chi$Start.Time), '%m')
ny$Start.Month <- format(as.Date(ny$Start.Time), '%m')
wash$Start.Month <- format(as.Date(wash$Start.Time), '%m')

month.sum <- function(x){
    df <- x %>% group_by(month=x$Start.Month) %>% filter(!is.na(month)) %>% summarise(mon.count=n())
    return(df)
}
chi_month <- month.sum(chi)
chi_month
ny_month <- month.sum(ny)
ny_month
wash_month <- month.sum(wash)
wash_month

df <- data.frame(Month=chi_month$month, Chicago=chi_month$mon.count, New_York=ny_month$mon.count, Washington=wash_month$mon.count)
df

newdf <- df %>% gather(key=City, value=Count, Chicago, New_York, Washington)
newdf

common_month_plot <- ggplot(newdf, aes(City, Count, fill=Month)) + geom_col(position = "dodge") + ggtitle('Months for each City') + theme(plot.title = element_text(hjust = 0.5))

common_month_plot

mean_june <- mean(c(df[6,2],df[6,3],df[6,4]))
mean_june

From the above bar plot, we can conculde that the common month for all three cities is June with 2816 for Chicago, 14000 for NYC, and 20335 for Washinfton. Mean of common month is 12383.7. The contrasting phenomenon could infer the discrepancy of traffic conditions. 

What's the average travel time in all the three cities

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

library(readr);
library(tidyr);
library(plyr);
library(dplyr);
library(ggplot2)

trip_dur <- list(chi$Trip.Duration, ny$Trip.Duration[!is.na(ny$Trip.Duration)], wash$Trip.Duration[!is.na(wash$Trip.Duration)]) 
avg_trip <- lapply(trip_dur, mean)
avg_trip

df <- data.frame(City=c('Chicago', 'New_York', 'Washington'), avg_trip=c(avg_trip[[1]],avg_trip[[2]],avg_trip[[3]]))

ggplot(df, aes(City, avg_trip, fill=City)) + geom_col(position='dodge') + ggtitle('Average Travel Time') + theme(plot.title = element_text(hjust = 0.5))

In sum, average trip timr in Washington is the highest amoung all three cities.And mean of Chicago and NYC is similar, which is around 900

What are the counts of each gender (only available for NYC and Chicago)?

ny <- read.csv('new_york_city.csv')
chi <- read.csv('chicago.csv')

library(readr);
library(tidyr);
library(dplyr);
library(ggplot2)

gender_ct_chi <- chi %>% group_by(Gender) %>% summarize(chi.gender.ct=n())
gender_ct_chi
gender_ct_ny <- ny %>% group_by(Gender) %>% summarize(ny.gender.ct=n())
gender_ct_ny

df <- data.frame(Gender=c('Unkown', 'Female', 'Male'), NYC=gender_ct_ny$ny.gender.ct, Chicago=gender_ct_chi$chi.gender.ct)
newdf <- df %>% gather(key=City, value=Count, NYC, Chicago)

ggplot(newdf, aes(City, Count, fill=Gender)) + geom_col(position='dodge') + ggtitle('Gender by Cities') + theme(plot.title = element_text(hjust = 0.5))

The result is obviously contrasting, the common feature in two cities is that the number of identified male is larger than that of female significantly with reported 37201 males in NYC.

system('python -m nbconvert Explore_bikeshare_data.ipynb')
