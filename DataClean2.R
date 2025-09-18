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
convert_military_time <- function(time_col, flight_date) {
  time_str <- sprintf("%04d", time_col) # pad (e.g. 530 → 0530)
  time_str[!grepl("^[0-2][0-9][0-5][0-9]$", time_str)] <- NA # invalid → NA
  
  hours <- as.integer(substr(time_str, 1, 2))
  mins  <- as.integer(substr(time_str, 3, 4))
  
  hours[hours > 23] <- NA
  mins[mins > 59] <- NA
  
  dt <- ymd(flight_date) + hours(hours) + minutes(mins)
  return(dt)
}



flightData_clean <- flightData_clean %>%
  mutate(
    FLIGHT_DATE = ymd(paste(YEAR, MONTH, DAY, sep = "-")),
    
    # Scheduled departure
    SCHEDULED_DEPARTURE = ifelse(!is.na(SCHEDULED_DEPARTURE),
                                 convert_military_time(SCHEDULED_DEPARTURE, FLIGHT_DATE),
                                 NA),
    
    # Scheduled arrival
    SCHEDULED_ARRIVAL = ifelse(!is.na(SCHEDULED_ARRIVAL),
                               convert_military_time(SCHEDULED_ARRIVAL, FLIGHT_DATE),
                               NA),
    
    # Wheels off
    WHEELS_OFF = ifelse(!is.na(WHEELS_OFF),
                        convert_military_time(WHEELS_OFF, FLIGHT_DATE),
                        NA),
    
    # Wheels on
    WHEELS_ON = ifelse(!is.na(WHEELS_ON),
                       convert_military_time(WHEELS_ON, FLIGHT_DATE),
                       NA),
    
    # Air time
    AIR_TIME = ifelse(is.na(AIR_TIME) & !is.na(WHEELS_ON) & !is.na(WHEELS_OFF),
                      as.numeric(difftime(WHEELS_ON, WHEELS_OFF, units = "mins")),
                      AIR_TIME),
    
    # Taxi in
    TAXI_IN = ifelse(is.na(TAXI_IN) & !is.na(ARRIVAL_TIME) & !is.na(WHEELS_ON),
                     as.numeric(difftime(ARRIVAL_TIME, WHEELS_ON, units = "mins")),
                     TAXI_IN),
    
    # Taxi out
    TAXI_OUT = ifelse(is.na(TAXI_OUT) & !is.na(WHEELS_OFF) & !is.na(DEPARTURE_TIME),
                      as.numeric(difftime(WHEELS_OFF, DEPARTURE_TIME, units = "mins")),
                      TAXI_OUT),
    
    # Arrival time
    ARRIVAL_TIME = ifelse(is.na(ARRIVAL_TIME) & !is.na(WHEELS_ON) & !is.na(TAXI_IN),
                          WHEELS_ON + minutes(TAXI_IN),
                          ARRIVAL_TIME),
    
    # Departure time
    DEPARTURE_TIME = ifelse(is.na(DEPARTURE_TIME) & !is.na(WHEELS_OFF) & !is.na(TAXI_OUT),
                            WHEELS_OFF - minutes(TAXI_OUT),
                            DEPARTURE_TIME),
    
    # Elapsed time
    ELAPSED_TIME = ifelse(is.na(ELAPSED_TIME) & !is.na(AIR_TIME) & !is.na(TAXI_IN) & !is.na(TAXI_OUT),
                          (AIR_TIME + TAXI_IN + TAXI_OUT),
                          round(ELAPSED_TIME, 1)),
    
    # Arrival delay
    ARRIVAL_DELAY = ifelse(is.na(ARRIVAL_DELAY) & !is.na(ARRIVAL_TIME) & !is.na(SCHEDULED_ARRIVAL),
                           as.numeric(difftime(ARRIVAL_TIME, SCHEDULED_ARRIVAL, units = "mins")),
                           ARRIVAL_DELAY),
    
    # Departure delay
    DEPARTURE_DELAY = ifelse(is.na(DEPARTURE_DELAY) & !is.na(DEPARTURE_TIME) & !is.na(SCHEDULED_DEPARTURE),
                             as.numeric(difftime(DEPARTURE_TIME, SCHEDULED_DEPARTURE, units = "mins")),
                             DEPARTURE_DELAY)
  )