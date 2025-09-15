library(ggplot2)
library(data.table)
library(crayon)
library(readxl)
library(RSQLite)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)

setwd("C:\\Users\\User\\OneDrive\\Desktop\\Project\\R Programming\\R Data\\2 Dataset")
flightData <- read.csv("flights.csv", header = TRUE)
flightData
acData <- read.csv("iata_airline_codes.csv")
arData <- read.csv("iata_airport_codes.csv")

#Data cleaning
#use box plot to identify outliers?
flightData_clean <- distinct(flightData)
flightData_clean

fix_military_time <- function(time_col) 
{
  # Extract hours and minutes
  hours <- time_col %/% 100
  minutes <- time_col %% 100
  
  # Correct minutes > 59 by subtracting 60 and adding 1 to hours
  overflow_min <- minutes > 59
  minutes[overflow_min] <- minutes[overflow_min] - 60
  hours[overflow_min] <- hours[overflow_min] + 1
  
  # Correct hours > 23 by subtracting 24
  hours <- ifelse(hours > 23, hours - 24, hours)
  
  # Recombine hours and minutes
  time_col <- hours * 100 + minutes
  
  # Set negative values to NA
  time_col <- ifelse(time_col < 0, NA, time_col)
  
  return(time_col)
}
test_times <- c(1230, 2400, 1679, -50, 2359)
fix_military_time(test_times)

#	TAXI_OUT: The time duration elapsed between departure from the ORIGIN_AIRPORT and WHEELS_OFF.
flightData_clean$TAXI_OUT <- ifelse(is.na(flightData_clean$TAXI_OUT), mean(flightData_clean$TAXI_OUT, na.rm =  TRUE),
                                    round(flightData_clean$TAXI_OUT,1))
#TAXI_IN: Time duration elapsed between WHEELS_ON and gate arrival at DESTINATION_AIRPORT.
flightData_clean$TAXI_IN<- ifelse(is.na(flightData_clean$TAXI_IN), mean(flightData_clean$TAXI_IN, na.rm =  TRUE),
                                    round(flightData_clean$TAXI_IN,1))
#	WHEELS_ON: Time point that the aircraft's wheels touch the ground.
flightData_clean$WHEELS_ON <- ifelse(is.na(flightData_clean$WHEELS_ON), mean(flightData_clean$WHEELS_ON, na.rm =  TRUE),
                                      round(flightData_clean$WHEELS_ON,1))

#WHEELS_OFF: Time point that the aircraft's wheels leave the ground.
flightData_clean$WHEELS_OFF <- ifelse(is.na(flightData_clean$WHEELS_OFF), mean(flightData_clean$WHEELS_OFF, na.rm =  TRUE),
                                      round(flightData_clean$WHEELS_OFF,1))

#DEPARTURE_TIME: WHEEL_OFF - TAXI_OUT.
flightData_clean$DEPARTURE_TIME <- ifelse(is.na(flightData_clean$DEPARTURE_TIME) & !is.na(flightData_clean$WHEELS_OFF) & !is.na(flightData_clean$TAXI_OUT),
                                          (flightData_clean$WHEELS_OFF- flightData_clean$TAXI_OUT),
                                          round(flightData_clean$DEPARTURE_TIME,1))

#DEPARTURE_DELAY: Total delay on departure.
flightData_clean$DEPARTURE_DELAY <- ifelse(is.na(flightData_clean$DEPARTURE_DELAY) & !is.na(flightData_clean$SCHEDULED_DEPARTURE) & !is.na(flightData_clean$DEPARTURE_TIME),
                                           (flightData_clean$SCHEDULED_DEPARTURE- flightData_clean$DEPARTURE_TIME),
                                           round(flightData_clean$DEPARTURE_DELAY))

#AIR_TIME: Time duration between WHEELS_OFF and WHEELS_ON time.
flightData_clean$AIR_TIME <- ifelse(is.na(flightData_clean$AIR_TIME) & !is.na(flightData_clean$WHEELS_ON) & !is.na(flightData_clean$WHEELS_OFF),
                                    (flightData_clean$WHEELS_OFF- flightData_clean$WHEELS_OFF),
                                    abs(round(flightData_clean$ELAPSED_TIME,1)))


#ELAPSED_TIME: AIR_TIME + TAXI_IN + TAXI_OUT
flightData_clean$ELAPSED_TIME <- ifelse(is.na(flightData_clean$ELAPSED_TIME) & !is.na(flightData_clean$AIR_TIME) & !is.na(flightData_clean$TAXI_IN) & !is.na(flightData_clean$TAXI_OUT),
                                        (flightData_clean$AIR_TIME + flightData_clean$TAXI_IN + flightData_clean$TAXI_OUT),
                                        round(flightData_clean$ELAPSED_TIME,1))


#ARRIVAL_TIME: WHEELS_ON + TAXI_IN.
flightData_clean$ARRIVAL_TIME <- ifelse(is.na(flightData_clean$ARRIVAL_TIME) & !is.na(flightData_clean$WHEELS_ON) & !is.na(flightData_clean$TAXI_IN),
                                        (flightData_clean$WHEELS_ON + flightData_clean$TAXI_IN),
                                        round(flightData_clean$ARRIVAL_TIME,1))

#	ARRIVAL_DELAY: ARRIVAL_TIME - SCHEDULED_ARRIVAL
flightData_clean$ARRIVAL_DELAY <- ifelse(is.na(flightData_clean$ARRIVAL_DELAY) & !is.na(flightData_clean$ARRIVAL_TIME) & !is.na(flightData_clean$SCHEDULED_ARRIVAL),
                                         (flightData_clean$ARRIVAL_TIME - flightData_clean$SCHEDULED_ARRIVAL),
                                         round(flightData_clean$ARRIVAL_DELAY,1))

flightData_clean$SCHEDULED_DEPARTURE <- fix_military_time(flightData_clean$SCHEDULED_DEPARTURE)
flightData_clean$DEPARTURE_TIME <- fix_military_time(flightData_clean$DEPARTURE_TIME)
flightData_clean$SCHEDULED_ARRIVAL <- fix_military_time(flightData_clean$SCHEDULED_ARRIVAL)
#flightData_clean$ARRIVAL_TIME <- fix_military_time(flightData_clean$ARRIVAL_TIME)
flightData_clean$WHEELS_OFF <- fix_military_time(flightData_clean$WHEELS_OFF)
flightData_clean$WHEELS_ON <- fix_military_time(flightData_clean$WHEELS_ON)

#Ensure that it is four numbers making up the time data
#flightData_clean$DEPARTURE_TIME <- sprintf("%04d", flightData_clean$DEPARTURE_TIME)
#flightData_clean$ARRIVAL_TIME <- sprintf("%04d", flightData_clean$ARRIVAL_TIME)

#fill in Air system delay (0 to 60 minutes)
#na_pos <- is.na(flightData_clean$AIR_SYSTEM_DELAY)
#flightData_clean$AIR_SYSTEM_DELAY[na_pos] <- sample(0:60, sum(na_pos), replace = TRUE)

#Data cleaning for Delays

#check missing Data
is.na(flightData_clean)
sum(is.na(flightData_clean))

invalid_times <- subset(flightData_clean, DEPARTURE_TIME > 2359 | ARRIVAL_TIME > 2359)
nrow(invalid_times)  # Should be 0 if everything's clean

sum(flightData_clean$ARRIVAL_DELAY < 0)

#Data Preprocessing
flightData_clean <- mutate(flightData_clean, Delayed = ifelse(flightData_clean$ARRIVAL_DELAY > 0,1,0))

delayed_flights <- which(flightData_clean$Delayed == 1)
delayed_flights


ggplot(flightData_clean, aes(x= AIR_SYSTEM_DELAY, y= ARRIVAL_DELAY)) +
  geom_point(alpha=0.3)+
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Impact of Air System Delay on Arrival Delay",
       x = "Air System Delay (minutes)",
       y = "Arrival Delay (minutes)")

