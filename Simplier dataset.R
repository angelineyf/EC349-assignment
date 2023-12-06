# Use only review, users and business data

#Load packages
library(dplyr)
library(tidyverse)
library(readr)
library(jsonlite)
library(caret)
library(stringr)
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

#Load small data
review_data  <- load(file='yelp_review_small.Rda') 
user_data  <- load(file='yelp_user_small.Rda') 

#Preparing data

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

# Drop the text column
review_data_small <- review_data_small %>%
  select(-text)

# Keep only relevant variables in business_data
business_data <- business_data %>%
  select(business_id, name, city, postal_code, latitude, longitude, stars, review_count)

## merge review data with all other data
master_data <- review_data_small %>%
  outer_join(user_data_small, by = "user_id") %>%
  outer_join(business_data, by = "business_id")

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

# Copy new dataset for 1st specification
model1_data <- master_data %>%
  select(stars.x, review_id, sentiment_score, review_count.x, useful.y, average_stars, city, postal_code, stars.y, review_count.y, is_elite)

#Modelling 1: Ridge Regression

# Set seed for reproducibility
set.seed(1)

# Split the data into training and test sets
sample_size <- 10000
test_indices <- sample(1:nrow(model1_data), sample_size)
test_data <- model1_data[test_indices, ]
train_data <- model1_data[-test_indices, ]

# Specify the model formula
formula <- as.formula("stars.x ~ sentiment_score + review_count.x + useful.y + average_stars + city + postal_code + stars.y + review_count.y + is_elite")

# Convert factors to dummy variables
train_matrix <- model.matrix(formula, data = train_data)
test_matrix <- model.matrix(formula, data = test_data)

# Define the response variable
train_response <- train_data$stars.x
test_response <- test_data$stars.x

# Fit the ridge regression model
fit <- glmnet(train_matrix, train_response, alpha = 0, lambda = NULL)

# Perform cross-validation to find the optimal lambda
cv_fit <- cv.glmnet(train_matrix, train_response, alpha = 0)
optimal_lambda <- cv_fit$lambda.min

# Refit the model with the optimal lambda
fit <- glmnet(train_matrix, train_response, alpha = 0, lambda = optimal_lambda)

# Predict on the test set
predictions <- predict(fit, newx = test_matrix)

# Calculate the mean squared error
mse <- mean((test_response - predictions)^2)
