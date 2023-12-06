#Load packages
library(dplyr)
library(tidyverse)
library(readr)
library(jsonlite)
library(caret)
library(stringr)
library(purrr)
library(tidytext)
library(textdata)
library(glmnet)

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

#Cleaning & Rectangling data

#create sentiment scores from review text
## Get the sentiment lexicon
sentiments <- lexicon_afinn()
## Tokenise the text
tokens <- review_data_small %>%
  unnest_tokens(word, text)
## Calculate sentiment scores
sentiment_scores <- tokens %>%
  inner_join(sentiments) %>%
  group_by(review_id) %>%
  summarise(sentiment_score = sum(value))
#Add sentiment scores to review_data_small
review_data_small <- left_join(review_data_small, sentiment_scores, by = "review_id")

##Transform date variables to dttm format
review_data_small$date <- parse_datetime(review_data_small$date, format = "%Y-%m-%d %H:%M:%S", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)
user_data_small$yelping_since <- parse_datetime(user_data_small$yelping_since, format = "%Y-%m-%d %H:%M:%S", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)
tip_data$date <- parse_datetime(tip_data$date, format = "%Y-%m-%d %H:%M:%S", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)

# Drop the text column
review_data_small <- review_data_small %>%
  select(-text)

## Flatten attributes and hours in business_data
attributes <- flatten(business_data$attributes)
business_data <- cbind(business_data, attributes)

## Covert string variables to boolean
business_data <- business_data %>% mutate_at(vars(ByAppointmentOnly, BusinessAcceptsCreditCards, RestaurantsTakeOut, RestaurantsDelivery, Caters, RestaurantsReservations, OutdoorSeating, GoodForKids, DogsAllowed, RestaurantsGoodForGroups), as.logical)

business_data <- business_data %>%
  mutate(FreeWiFi = ifelse(WiFi == "u'free'", "true", 
                           ifelse(WiFi == "u'no'", "false", NA)))

business_data$FreeWiFi <- as.logical(business_data$FreeWiFi) # Convert the 'FreeWiFi' column to logical

## Globalise business category variable into factors
# Define the list of business categories (source: https://blog.yelp.com/businesses/yelp_category_list/)
business_category <- c("Active Life", "Arts & Entertainment", "Automotive", "Beauty & Spas", "Education", "Event Planning & Services", "Financial Services", "Food", "Health & Medical", "Home Services", "Hotels & Travel", "Local Flavor", "Local Services", "Mass Media", "Nightlife", "Pets", "Professional Services", "Public Services & Government", "Real Estate", "Religious Organizations", "Restaurants", "Shopping")

# Create a new variable that maps the business category
business_data <- business_data %>%
  mutate(category = map_chr(categories, ~str_extract(., paste(business_category, collapse = "|"))))
  
# Keep only relevant variables in business_data
business_data <- business_data %>%
  select(business_id, name, category, city, state, postal_code, latitude, longitude, stars, review_count, ByAppointmentOnly, BusinessAcceptsCreditCards, RestaurantsPriceRange2, RestaurantsTakeOut, RestaurantsDelivery, Caters, FreeWiFi, BusinessParking, WheelchairAccessible, Ambience, RestaurantsReservations, OutdoorSeating, GoodForMeal, GoodForKids, DogsAllowed, Alcohol, RestaurantsAttire, RestaurantsGoodForGroups, NoiseLevel)

##Generate frequency of business check-ins to indicate users engagement. 
##Check-ins are usually motivated by offers initiated by the businesses. 
##Business with more frequent check-ins are more likely to receive high reviews as these reviews were incentivised by promotional deals.
checkin_data$checkin_freq <- sapply(checkin_data$date, length)

#Drop checkin_data$date
checkin_data <- checkin_data %>%
  select(-date)


##Create a new variable to count the number of friends a user has
#user_data_small$friends <- strsplit(user_data_small$friends, split = ",")
#user_data_small$friends_count <- sapply(user_data_small$friends, length)
#friends variable may be useful if we could map the star ratings given by friends, but such procedure can be too computational heavy and hence I decided to omit it (for now). 

#Drop user_data_small$friends
#user_data_small <- user_data_small %>% #Dont drop first
#  select(-friends)

#Drop text in tip_data
tip_data <- tip_data %>%
  mutate(tip = 1)
tip_data <- tip_data %>%
  select(-text)


## merge business data and checkin data
merge_business <- business_data %>%
  left_join(checkin_data, by = "business_id")

## merge review data with all other data
master_data <- review_data_small %>%
  left_join(user_data_small, by = "user_id") %>%
  left_join(tip_data, by = c("user_id", "business_id", "date")) %>%
  left_join(merge_business, by = "business_id")

## replace tip == 0 if NA
master_data <- master_data %>%
  mutate(tip = if_else(is.na(tip), 0, tip))

##Create a dummy variable `is_elite` to indicate whether the user is an elite at the point when writing the review
# Convert review date to year format
master_data$review_year <- as.numeric(format(as.Date(master_data$date), "%Y"))

# Create a function to check if review year is in elite years
is_elite <- function(elite, review_year) {
  # Check if elite_years is missing
  if (is.na(elite)) {
    return(0)
  }
  
  elite_years_vector <- as.numeric(str_split(elite, ",")[[1]])
  return(as.integer(review_year %in% elite_years_vector))
}

# Apply the function to each row
master_data$is_elite <- mapply(is_elite, master_data$elite, master_data$review_year)

# Drop elite and review year
master_data <- master_data %>%
  select(-elite, -review_year)


#summary 
glimpse(master_data)
review_train <- na.omit(review_train)

#if u have more time: turn RestaurantPriceRange2 into <dbl>, WheelchairAccessible into <lgl>, Alcohol & NoiseLevel into <list>, RestaurantsAttire into <lgl> if casual, expand dummy for "BusinessParking" "Ambience" "GoodForMeal"



#Modelling
#First split data into training and test
set.seed(1)  #for reproducibility
rows <- sample(nrow(master_data))  #Shuffle row indices: rows
shuffled_review <- master_data[rows, ]  #Randomly order data
review_test <- shuffled_review[1:10000, ]  #test dataset
review_train <- shuffled_review[10001:nrow(shuffled_review), ]  #training dataset

#reduce size of training data
review_train <- review_train %>%
  select(stars.x, review_id, user_id, business_id, sentiment_score, review_count.x, useful.y, average_stars, tip, city, postal_code, stars.y, review_count.y, checkin_freq, is_elite)

#Since stars rating is a discrete variable, a regression model will suffice to predict 
lm_model <- lm(stars.x ~ sentiment_score + review_count.x + useful.y + average_stars + tip + city +  postal_code + stars.y + review_count.y + checkin_freq + is_elite, review_train)  #Fit linear model on training dataset
predict <- predict(lm_model, review_test)  #Predict on test dataset
error <- predict - review_test$stars.x     #Compute errors
rmse <- sqrt(mean(error^2))                #Calculate RMSE
rmse

