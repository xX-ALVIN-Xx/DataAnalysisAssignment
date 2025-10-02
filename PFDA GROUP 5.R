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

#JimVoo
setwd("C:\\Users\\User\\OneDrive\\Desktop\\Project\\R Programming\\R Data\\2 Dataset")
#LiewZerShuen
setwd("C:\\Users\\zersh\\Desktop\\PFDA_Assignment_Datasets\\2 Dataset")
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
                                    (flightData_clean$WHEELS_ON-flightData_clean$WHEELS_OFF),
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
flightData_clean$WHEELS_OFF <- fix_military_time(flightData_clean$WHEELS_OFF)
flightData_clean$WHEELS_ON <- fix_military_time(flightData_clean$WHEELS_ON)


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
head(delayed_flights,10)

#Filtering and distributing NA delays
# Define delay columns
delay_cols <- c("AIR_SYSTEM_DELAY", "SECURITY_DELAY", "AIRLINE_DELAY", "LATE_AIRCRAFT_DELAY", "WEATHER_DELAY")

# Filter complete cases with valid ARRIVAL_DELAY
complete_cases <- flightData_clean %>%
  filter(Delayed == 1, !if_any(all_of(delay_cols), is.na), ARRIVAL_DELAY > 0)

# Calculate average ratio of each delay type to ARRIVAL_DELAY
avg_ratios <- sapply(delay_cols, function(col) {
  mean(complete_cases[[col]] / complete_cases$ARRIVAL_DELAY, na.rm = TRUE)
})

# Normalize ratios to sum to 1
avg_ratios <- avg_ratios / sum(avg_ratios)

for (i in delayed_flights) {
  na_delays <- delay_cols[is.na(flightData_clean[i, delay_cols])]
  
  if (length(na_delays) > 0 && !is.na(flightData_clean$ARRIVAL_DELAY[i]) && flightData_clean$ARRIVAL_DELAY[i] > 0) {
    # Use only relevant ratios for missing columns
    relevant_ratios <- avg_ratios[na_delays]
    relevant_ratios <- relevant_ratios / sum(relevant_ratios)  # re-normalize
    
    allocated <- round(flightData_clean$ARRIVAL_DELAY[i] * relevant_ratios, 1)
    
    # Assign the split values to the NA columns
    flightData_clean[i, na_delays] <- allocated
  }
}


##############################################################

#Jim Voo Zhen Zhan #TP084679
#Objective 1 : To analyse the impact of air system delay on arrival delay and determine whether congestion 
#in air traffic control contributes significantly to late arrivals. 


#1.0 Air System Delay vs Arrival Delay
ggplot(flightData_clean, aes(x= AIR_SYSTEM_DELAY, y= ARRIVAL_DELAY)) +
  geom_point(alpha=0.3)+
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Impact of Air System Delay on Arrival Delay",
       x = "Air System Delay (minutes)",
       y = "Arrival Delay (minutes)")

# Create a new column for total operational delay
flightData_clean <- flightData_clean %>%
  mutate(TOTAL_DELAY = rowSums(select(., AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY, WEATHER_DELAY), na.rm = TRUE),
         AIR_SYSTEM_SHARE = round(AIR_SYSTEM_DELAY / TOTAL_DELAY, 2))
flightData_clean$TOTAL_DELAY

#1.1 Remove rows where TOTAL_DELAY is 0 to avoid division by zero
flightData_clean <- flightData_clean %>% filter(TOTAL_DELAY > 0)

ggplot(flightData_clean, aes(x = AIR_SYSTEM_SHARE)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  labs(title = "Frequency Distribution of Air System Delay Share",
       x = "Proportion of Air System Delay in Total Delay",
       y = "Number of Flights") +
  theme_minimal()

#1.2 Relationship Between Air Time and Air System Delay
#use cor to test for association between a pair of variables, complete.obs is used to disregard NA values
cor.test(flightData_clean$AIR_TIME, flightData_clean$AIR_SYSTEM_DELAY, use = "complete.obs")

ggplot(flightData_clean, aes(x = AIR_TIME, y = AIR_SYSTEM_DELAY)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  geom_smooth(method = "lm", color = "orange") +
  labs(title = "Air Time vs Air System Delay",
       x = "Air Time (minutes)",
       y = "Air System Delay (minutes)")


#2.0 Weather Delay vs Air System Delay
ggplot(flightData_clean, aes(x = WEATHER_DELAY, y = AIR_SYSTEM_DELAY)) +
  geom_point(alpha = 0.3, color = "darkgreen  ") +
  geom_smooth(method = "lm", color = "orange") +
  labs(title = "Weather Delay vs Air System Delay",
       x = "Weather Delay (minutes)",
       y = "Air System Delay (minutes)")
#2.1 Seasonal Trends
ggplot(flightData_clean, aes(x = factor(MONTH), y = AIR_SYSTEM_DELAY)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Monthly Variation in Air System Delay",
       x = "Month",
       y = "Air System Delay (minutes)")

#3.0 Time Dependency of Air System Delay
flightData_clean$DEP_HOUR <- flightData_clean$SCHEDULED_DEPARTURE %/% 100

ggplot(flightData_clean, aes(x = factor(DEP_HOUR), y = AIR_SYSTEM_DELAY)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Air System Delay by Scheduled Departure Hour",
       x = "Scheduled Departure Hour",
       y = "Air System Delay (minutes)")

#3.1 DAY OF THE WEEK TREND
ggplot(flightData_clean, aes(x = factor(DAY_OF_WEEK), y = AIR_SYSTEM_DELAY)) +
  geom_boxplot(fill = "lightyellow") +
  labs(title = "Air System Delay by Day of Week",
       x = "Day of Week",
       y = "Air System Delay (minutes)")

ggplot(flightData_clean, aes(x = factor(DAY_OF_WEEK), y = AIR_SYSTEM_DELAY)) +
  geom_histogram(fill )


################################################################################

#Liew Zer Shuen TP076363
#Objective: To analyse the impact of Weather Delay on Arrival Delay 
#and determine whether adverse weather conditions contribute 
#significantly to late arrivals.

# 1.0 Weather Delay vs Arrival Delay
ggplot(flightData_clean, aes(x = WEATHER_DELAY, y = ARRIVAL_DELAY)) +
  geom_point(alpha = 0.3, color = "skyblue") +
  geom_smooth(method = "lm", color = "darkblue") +
  labs(title = "Impact of Weather Delay on Arrival Delay",
       x = "Weather Delay (minutes)",
       y = "Arrival Delay (minutes)")

# 1.1 Contribution of Weather Delay to Total Delay
flightData_clean <- flightData_clean %>%
  mutate(TOTAL_DELAY = rowSums(select(., AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY, WEATHER_DELAY), na.rm = TRUE),
         WEATHER_SHARE = round(WEATHER_DELAY / TOTAL_DELAY, 2))

# Remove rows where TOTAL_DELAY is 0 to avoid division by zero
flightData_clean <- flightData_clean %>% filter(TOTAL_DELAY > 0)

ggplot(flightData_clean, aes(x = WEATHER_SHARE)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Weather Delay Share in Total Delay",
       x = "Proportion of Weather Delay",
       y = "Number of Flights") +
  theme_minimal()

# 2.0 Weather Delay vs Departure Delay
ggplot(flightData_clean, aes(x = WEATHER_DELAY, y = DEPARTURE_DELAY)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  geom_smooth(method = "lm", color = "orange") +
  labs(title = "Weather Delay vs Departure Delay",
       x = "Weather Delay (minutes)",
       y = "Departure Delay (minutes)")

# 2.1 Seasonal Trends (Monthly Weather Delays)
ggplot(flightData_clean, aes(x = factor(MONTH), y = WEATHER_DELAY)) +
  geom_boxplot(fill = "lightsteelblue") +
  labs(title = "Monthly Variation in Weather Delay",
       x = "Month",
       y = "Weather Delay (minutes)")

# 3.0 Time Dependency of Weather Delay
flightData_clean$DEP_HOUR <- flightData_clean$SCHEDULED_DEPARTURE %/% 100

ggplot(flightData_clean, aes(x = factor(DEP_HOUR), y = WEATHER_DELAY)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Weather Delay by Scheduled Departure Hour",
       x = "Scheduled Departure Hour",
       y = "Weather Delay (minutes)")

# 3.1 Day of Week Trends
ggplot(flightData_clean, aes(x = factor(DAY_OF_WEEK), y = WEATHER_DELAY)) +
  geom_boxplot(fill = "khaki") +
  labs(title = "Weather Delay by Day of Week",
       x = "Day of Week",
       y = "Weather Delay (minutes)")

# 4.0 Weather Delay by Origin Airport
top_airports <- names(sort(table(flightData_clean$ORIGIN_AIRPORT), decreasing = TRUE))[1:10]
filtered_data <- subset(flightData_clean,
                        ORIGIN_AIRPORT %in% top_airports &
                          !is.na(WEATHER_DELAY) &
                          !is.na(DEPARTURE_DELAY))

ggplot(filtered_data, aes(x = ORIGIN_AIRPORT, y = WEATHER_DELAY)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.color = "red") +
  labs(title = "Weather Delay by Origin Airport (Top 10 Airports)",
       x = "Origin Airport",
       y = "Weather Delay (minutes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.1 Weather Delay vs Departure Delay by Airport
ggplot(filtered_data, aes(x = WEATHER_DELAY, y = DEPARTURE_DELAY, color = ORIGIN_AIRPORT)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Weather Delay vs Departure Delay by Origin Airport",
       x = "Weather Delay (minutes)",
       y = "Departure Delay (minutes)") +
  theme_minimal()

#############################################################