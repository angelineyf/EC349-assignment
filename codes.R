library(dplyr)
library(tidyverse)
library(readr)

#Pre-Processing Yelp Academic Data
library(jsonlite)

#Clear
cat("\014")  
rm(list=ls())

#Set Directory as appropriate
setwd("C:/Users/angel/OneDrive - University of Warwick/Year 3/EC349 Data Science/R projects/EC349-assignment/Yelp-datasets")

#Load .json Data
business_data <- stream_in(file("yelp_academic_dataset_business.json")) 
checkin_data  <- stream_in(file("yelp_academic_dataset_checkin.json")) 
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json")) 

#Load small data
review_data  <- load(file='yelp_review_small.Rda') 
user_data  <- load(file='yelp_user_small.Rda') 

#Merge data into a 2-dimensional dataset

#Rename "date" variable in checkin_data to avoid overlapping column names
checkin_data <- checkin_data %>%
  rename(checkin_date = date)

#merge business data and checkin data
merge_business_data <- business_data %>%
  left_join(checkin_data, by = "business_id")

#merge review data with all other data
merged_data <- review_data_small %>%
  left_join(user_data_small, by = "user_id") %>%
  left_join(tip_data, by = c("user_id", "business_id", "date")) %>%
  left_join(merge_business_data, by = "business_id")

#summary 
glimpse(merged_data)

##Data cleaning

merged_data$date <- parse_datetime(merged_data$date, format = "%Y-%m-%d %H:%M:%S", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)
merged_data$yelping_since <- parse_datetime(merged_data$yelping_since, format = "%Y-%m-%d %H:%M:%S", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)
#Factorise `state`
#Do I need to convert the list in `elite` and `friends` into sub-dataframes/matrices?


