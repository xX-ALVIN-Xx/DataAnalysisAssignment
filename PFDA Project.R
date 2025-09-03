library(ggplot2)
library(data.table)
library(crayon)
library(readxl)
library(RSQLite)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyverse)

setwd("C:\\Users\\User\\OneDrive\\Desktop\\Project\\R Programming\\R Data\\2 Dataset")
flightData <- read.csv("flights.csv", header = TRUE)
flightData
acData <- read.csv("iata_airline_codes.csv")
arData <- read.csv("iata_airport_codes.csv")

#Data cleaning
#use box plot to identify outliers?
flightData_clean <- distinct(flightData)
flightData_clean
flightData_clean$DEPARTURE_TIME <- ifelse(is.na(flightData_clean$DEPARTURE_TIME), mean(flightData_clean$DEPARTURE_TIME, na.rm = TRUE),
                                          round(flightData_clean$DEPARTURE_TIME,0))
flightData_clean$DEPARTURE_DELAY <- ifelse(is.na(flightData_clean$DEPARTURE_DELAY), mean(flightData_clean$DEPARTURE_DELAY, na.rm =  TRUE),
                                           round(flightData_clean$DEPARTURE_DELAY,0))
flightData_clean$TAXI_OUT <- ifelse(is.na(flightData_clean$TAXI_OUT), mean(flightData_clean$TAXI_OUT, na.rm =  TRUE),
                                    round(flightData_clean$TAXI_OUT,0))
flightData_clean$WHEELS_OFF <- ifelse(is.na(flightData_clean$WHEELS_OFF), mean(flightData_clean$WHEELS_OFF, na.rm =  TRUE),
                                      round(flightData_clean$WHEELS_OFF,0))
#ELAPSED_TIME: AIR_TIME + TAXI_IN + TAXI_OUT
flightData_clean$ELAPSED_TIME <- ifelse(is.na(flightData_clean$WHEELS_OFF), mean(flightData_clean$WHEELS_OFF, na.rm =  TRUE),
                                        round(flightData_clean$WHEELS_OFF,0))
flightData_clean$ARRIVAL_DELAY <-


#check missing Data
is.na(flightData_clean)
sum(is.na(flightData_clean))