# Justin Davis  
# 06/25/25
# Data Practium
# Creating a CSV file 

library(tidyverse)
library(ggplot2)
library(tsibble)
library(feasts)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(hms)
library(readr)
library(lubridate)
library(xml2)

#set working directory 
setwd("/Users/justin/Desktop/Data Science - Summer 2025/RegenRate")

#getting working directory
getwd()

#load in export file from apple health
health <- read_xml("export.xml")

#extract all records
records <- xml_find_all(health, ".//Record")

# Extract all attribute names from the first few records (or all)
attrs_list <- xml_attrs(records)

# Get all unique attribute names used across all records
all_attrs <- unique(unlist(lapply(attrs_list, names)))

#print(all_attrs)

#looking at all of the unique types for the type column
records_type <- unique(xml_attr(records, "type"))

#print(records_type)

parse_mixed_datetime <- function(x) {
  dt <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  dt[is.na(dt)] <- as.POSIXct(x[is.na(dt)], format = "%Y-%m-%d %H:%M:%S %z", tz = "UTC")
  dt[is.na(dt)] <- as.POSIXct(x[is.na(dt)], format = "%Y-%m-%d", tz = "UTC")
  return(dt)
}

record <- tibble(
  type = xml_attr(records, "type"),
  source = xml_attr(records, "sourceName"),
  unit = xml_attr(records, "unit"),
  creation_date = xml_attr(records, "creationDate"),
  start_date = parse_mixed_datetime(xml_attr(records, "startDate")),
  end_date = parse_mixed_datetime(xml_attr(records, "endDate")),
  value = xml_attr(records, "value")
)

#unique(record$start_date)

# Filtering the types I want 
selected_types <- c(
  "HKQuantityTypeIdentifierHeartRate",
  "HKQuantityTypeIdentifierOxygenSaturation",
  "HKQuantityTypeIdentifierRespiratoryRate",
  "HKQuantityTypeIdentifierStepCount",
  "HKQuantityTypeIdentifierDistanceWalkingRunning",
  "HKQuantityTypeIdentifierBasalEnergyBurned",
  "HKQuantityTypeIdentifierActiveEnergyBurned",
  "HKQuantityTypeIdentifierAppleExerciseTime",
  "HKQuantityTypeIdentifierRestingHeartRate",
  "HKQuantityTypeIdentifierVO2Max",
  "HKQuantityTypeIdentifierWalkingHeartRateAverage",
  "HKQuantityTypeIdentifierAppleStandTime",
  "HKQuantityTypeIdentifierWalkingSpeed",
  "HKDataTypeSleepDurationGoal",
  "HKQuantityTypeIdentifierRunningStrideLength",
  "HKQuantityTypeIdentifierRunningVerticalOscillation",
  "HKQuantityTypeIdentifierRunningGroundContactTime",
  "HKQuantityTypeIdentifierHeartRateRecoveryOneMinute",
  "HKQuantityTypeIdentifierRunningPower",
  "HKQuantityTypeIdentifierRunningSpeed",
  "HKQuantityTypeIdentifierPhysicalEffort",
  "HKCategoryTypeIdentifierSleepAnalysis",
  "HKCategoryTypeIdentifierAppleStandHour",
  "HKQuantityTypeIdentifierHeartRateVariabilitySDNN"
)

filtered <- record %>% filter(type %in% selected_types)

# Rename types to readable metric names
type_labels <- c(
  "HKQuantityTypeIdentifierHeartRate" = "Heart Rate",
  "HKQuantityTypeIdentifierOxygenSaturation" = "Oxygen Saturation",
  "HKQuantityTypeIdentifierRespiratoryRate" = "Respiratory Rate",
  "HKQuantityTypeIdentifierStepCount" = "Steps",
  "HKQuantityTypeIdentifierDistanceWalkingRunning" = "Distance Walk/Run",
  "HKQuantityTypeIdentifierBasalEnergyBurned" = "Basal Energy Burned",
  "HKQuantityTypeIdentifierActiveEnergyBurned" = "Active Energy Burned",
  "HKQuantityTypeIdentifierAppleExerciseTime" = "Exercise Minutes",
  "HKQuantityTypeIdentifierRestingHeartRate" = "Resting Heart Rate",
  "HKQuantityTypeIdentifierVO2Max" = "VO2 Max",
  "HKQuantityTypeIdentifierWalkingHeartRateAverage" = "Walking Heart Rate Avg",
  "HKQuantityTypeIdentifierAppleStandTime" = "Stand Time",
  "HKQuantityTypeIdentifierWalkingSpeed" = "Walking Speed",
  "HKDataTypeSleepDurationGoal" = "Sleep Goal Duration",
  "HKQuantityTypeIdentifierRunningStrideLength" = "Running Stride Length",
  "HKQuantityTypeIdentifierRunningVerticalOscillation" = "Running Vertical Oscillation",
  "HKQuantityTypeIdentifierRunningGroundContactTime" = "Ground Contact Time",
  "HKQuantityTypeIdentifierHeartRateRecoveryOneMinute" = "HR Recovery (1 min)",
  "HKQuantityTypeIdentifierRunningPower" = "Running Power",
  "HKQuantityTypeIdentifierRunningSpeed" = "Running Speed",
  "HKQuantityTypeIdentifierPhysicalEffort" = "Physical Effort",
  "HKCategoryTypeIdentifierSleepAnalysis" = "Sleep Analysis",
  "HKCategoryTypeIdentifierAppleStandHour" = "Stand Hours",
  "HKQuantityTypeIdentifierHeartRateVariabilitySDNN" = "Heart Rate Variability"
)

# Add a readable name column
filtered <- filtered %>%
  mutate(metric_name = type_labels[type])

# Reorder columns for clarity
filtered <- filtered %>%
  select(metric_name, everything())

# Get UTC range for a local day
local_day <- as.Date("2025-06-21")
start_utc <- as.POSIXct(paste(local_day, "00:00:00"), tz = "America/New_York")
end_utc <- as.POSIXct(paste(local_day + 1, "00:00:00"), tz = "America/New_York") - 1

utc_start <- with_tz(start_utc, "UTC")
utc_end <- with_tz(end_utc, "UTC")

#cat("UTC range for local day:", utc_start, "to", utc_end, "\n")

# Shift all start_date values by -4 hours before converting to local time
filtered <- filtered %>%
  mutate(
    start_date_shifted = start_date + hours(-4),
    start_date_local = with_tz(start_date_shifted, tzone = "America/New_York"),
    date_local = as.Date(start_date_local)
  )

# Print a summary table: number of records for each local date
local_date_summary <- filtered %>%
  group_by(date_local) %>%
  summarise(n_records = n())
#print(local_date_summary)

# Example: View all records for a specific local date (e.g., 2025-06-21)
all_values_for_local_date <- filtered %>%
  filter(date_local == as.Date("2025-06-22")) %>%
  select(metric_name, start_date, start_date_shifted, start_date_local, value, everything())

#View(all_values_for_local_date)

# Remove unnecessary columns before saving
filtered <- filtered %>%
  select(-start_date, -end_date, -creation_date)

# Convert start_date_shifted to character for string operations
filtered$start_date_shifted <- as.character(filtered$start_date_shifted)

#Check the format of start_date_shifted
#cat("Sample of start_date_shifted values:\n")
#print(head(filtered$start_date_shifted, 10))

# Extract time of day more safely
filtered <- filtered %>%
  mutate(
    # Extract time portion (HH:MM:SS) from datetime string
    time_string = stringr::str_extract(start_date_shifted, "\\d{2}:\\d{2}:\\d{2}"),
    # Convert to hms, handling NA values
    time_of_day = hms::as_hms(time_string),
    # Add an 'hour' column for all rows
    hour = sprintf("%02d:00:00", hour(time_of_day))
  )

# Check for any remaining NA values in time_of_day
na_count <- sum(is.na(filtered$time_of_day))

if(na_count > 0) {
  cat("Sample of rows with NA time_of_day:\n")
  print(filtered %>% filter(is.na(time_of_day)) %>% select(start_date_shifted, time_string, time_of_day) %>% head(5))
}

# For Basal and Active Energy Burned, calculate hourly sum and join back to original data
energy_hourly_sums <- filtered %>%
  filter(metric_name %in% c("Basal Energy Burned", "Active Energy Burned", "Steps")) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(date_local, hour, metric_name) %>%
  summarise(hourly_sum = sum(value, na.rm = TRUE), .groups = "drop")

# Join the hourly sums back to the original data (only for those metrics)
filtered <- filtered %>%
  left_join(energy_hourly_sums, by = c("date_local", "hour", "metric_name"))

# View heart rate metrics for June 23, 2025
heart_rate_06_23 <- filtered %>%
  filter(date_local == as.Date("2025-06-23") & 
           metric_name %in% c("Heart Rate"))

#view(heart_rate_06_23)

# Filter to only include data from the past 2 years
two_years_ago <- Sys.Date() - years(2)
filtered <- filtered %>%
  filter(start_date_shifted >= two_years_ago)

#view(filtered)

# Save to CSV
write_csv(filtered, "apple_data.csv")
cat("Saved selected metrics to 'apple_data.csv'\n")
