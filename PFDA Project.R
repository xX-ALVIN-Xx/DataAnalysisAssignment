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
flightData_clean
#DEPARTURE_DELAY: Total delay on departure.
flightData_clean$DEPARTURE_DELAY <- ifelse(is.na(flightData_clean$DEPARTURE_DELAY), mean(flightData_clean$DEPARTURE_DELAY, na.rm =  TRUE),
                                           round(flightData_clean$DEPARTURE_DELAY,1))

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

#check missing Data
is.na(flightData_clean)
sum(is.na(flightData_clean))