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

## Flatten attributes and hours in business_data
attributes <- flatten(business_data$attributes)
business_data <- cbind(business_data, attributes)

## Globalise business category variable into factors
# Define the list of business categories (source: https://blog.yelp.com/businesses/yelp_category_list/)
business_category <- c("Active Life", "Arts & Entertainment", "Automotive", "Beauty & Spas", "Education", "Event Planning & Services", "Financial Services", "Food", "Health & Medical", "Home Services", "Hotels & Travel", "Local Flavor", "Local Services", "Mass Media", "Nightlife", "Pets", "Professional Services", "Public Services & Government", "Real Estate", "Religious Organizations", "Restaurants", "Shopping")

# Create a new variable that maps the business category
business_data <- business_data %>%
  mutate(category = map(categories, ~str_extract(., paste(business_category, collapse = "|"))))

# Keep only relevant variables in business_data
business_data <- business_data %>%
  select(business_id, city, state, postal_code, latitude, longitude, stars, review_count, is_open, ByAppointmentOnly, BusinessAcceptsCreditCards, RestaurantsPriceRange2, RestaurantsTakeOut, RestaurantsDelivery, Caters, WiFi, Ambience, RestaurantsReservations, OutdoorSeating, GoodForMeal, GoodForKids, DogsAllowed, RestaurantsGoodForGroups, NoiseLevel)

##Generate frequency of business check-ins to indicate users engagement. 
##Check-ins are usually motivated by offers initiated by the businesses. 
##Business with more frequent check-ins are more likely to receive high reviews as these reviews were incentivised by promotional deals.
checkin_data$date <- strsplit(checkin_data$date, split = ",")
checkin_data$checkin_freq <- sapply(checkin_data$date, length)

#Drop checkin_data$date
checkin_data <- checkin_data %>%
  select(-date)

##Transform date variables to dttm format
review_data_small$date <- parse_datetime(review_data_small$date, format = "%Y-%m-%d %H:%M:%S", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)
user_data_small$yelping_since <- parse_datetime(user_data_small$yelping_since, format = "%Y-%m-%d %H:%M:%S", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)

##Create a new variable to count the number of friends a user has
user_data_small$friends <- strsplit(user_data_small$friends, split = ",")
user_data_small$friends_count <- sapply(user_data_small$friends, length)

#Drop user_data_small$friends
user_data_small <- user_data_small %>%
  select(-friends)

#Drop text and compliment counts in tip_data
tip_data <- tip_data %>%
  mutate(tip = 1)
tip_data <- tip_data %>%
  select(user_id, business_id, tip)

#create sentiment scores from review text
sentiments <- textdata::lexicon_afinn()   # Get the sentiment lexicon
tokens <- review_data_small %>%           # Tokenise the text
  tidytext::unnest_tokens(word, text)
sentiment_scores <- tokens %>%            # Calculate sentiment scores
  inner_join(sentiments) %>%
  group_by(review_id) %>%
  summarise(sentiment_score = sum(score))
#Add sentiment scores to review_data_small
review_data_small <- left_join(review_data_small, sentiment_scores, by = "review_id")


## merge business data and checkin data
merge_business <- business_data %>%
  left_join(checkin_data, by = "business_id")

## merge review data with all other data
master_data <- review_data_small %>%
  left_join(user_data_small, by = "user_id") %>%
  left_join(tip_data, by = c("user_id", "business_id")) %>%
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




#Modelling
#First split data into training and test
set.seed(1)  #for reproducibility
rows <- sample(nrow(merged_data))  #Shuffle row indices: rows
shuffled_review <- merged_data[rows, ]  #Randomly order data
review_test <- shuffled_review[1:10000, ]  #test dataset
review_train <- shuffled_review[10001:nrow(shuffled_review), ]  #training dataset

#Linear Regression Model
lm_model <- lm(stars.x ~ ., review_train)  #Fit linear model on training dataset
predict <- predict(lm_model, review_test)  #Predict on test dataset
error <- predict - review_test$stars.x     #Compute errors
rmse <- sqrt(mean(error^2))                #Calculate RMSE
rmse

