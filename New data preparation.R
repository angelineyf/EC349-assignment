
# PREDICT USER'S STAR RATING BASED ON DATA ON THE USER PROFILE AND BUSINESS ATTRIBUTES

#Load packages
library(dplyr)
library(tidyverse)
library(readr)
library(jsonlite)
library(caret)
library(stringr)
library(glmnet)
library(lubridate)

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

#-------------------------------------------------------------------------------#

### 1. BUSINESS DATA
#Keep relevant business info
business_data <- business_data %>%
  select(business_id, name, city, state, postal_code, stars, review_count, categories, hours)

#Extract hours dataframe
hours <- flatten(business_data$hours)

#Is the business open on weekends (both days)?
hours <- hours %>%
  mutate(wkend_open = ifelse(!is.na(Saturday) & Saturday != "0:0-0:0" & !is.na(Sunday) & Sunday != "0:0-0:0", 1, 0))

#Keep only total_hours and wkend_open
hours <- hours %>%
  select(wkend_open)

#Combine hours with business data
business_data <- cbind(business_data, hours)

#Generate business rating power, take natural logarithm to shrink range of values
business_data <- business_data %>%
  mutate(rating_power = log(review_count*stars, base = exp(1)))

#Business category
# Define the list of business categories (source: https://blog.yelp.com/businesses/yelp_category_list/)
business_category <- c("Active Life", "Arts & Entertainment", "Automotive", "Beauty & Spas", "Education", "Event Planning & Services", "Financial Services", "Food", "Health & Medical", "Home Services", "Hotels & Travel", "Local Flavor", "Local Services", "Mass Media", "Nightlife", "Pets", "Professional Services", "Public Services & Government", "Real Estate", "Religious Organizations", "Restaurants", "Shopping")

# Create a new variable that maps the business category
business_data <- business_data %>%
  mutate(category = map_chr(categories, ~str_extract(., paste(business_category, collapse = "|"))))

#Drop categories & hours
business_data <- business_data %>%
  select(-categories, -hours)

# create dummy variables for each category
# install.packages("fastDummies")
library(fastDummies)

# Create dummy variables for each category
business_data <- fastDummies::dummy_cols(business_data, select_columns = "category")

#-------------------------------------------------------------------------------#

### 2. TIP DATA
# Count the number of tips received by each business
tip_count <- table(tip_data$business_id)

# Convert the table to a data frame
tip_count_df <- as.data.frame(tip_count)
colnames(tip_count_df) <- c("business_id", "tip_count")

# Merge the tip_count with the business_data
business_data <- full_join(business_data, tip_count_df, by = "business_id")

# If a business_id does not have a tip review, replace NA with 0
business_data$tip_count[is.na(business_data$tip_count)] <- 0

#-------------------------------------------------------------------------------#

### 3. CHECKIN DATA
# number of check-ins recorded for each business
checkin_data$checkin_freq <- sapply(checkin_data$date, length)

#Drop checkin_data$date
checkin_data <- checkin_data %>%
  select(-date)

#Merge with business_data
business_data <- full_join(business_data, checkin_data, by = "business_id")
business_data$checkin_freq[is.na(business_data$checkin_freq)] <- 0

#-------------------------------------------------------------------------------#

### 4. REVIEW & USER DATA
#Keep only y variable (stars rating)
review_data_small <- review_data_small %>%
  select(review_id, user_id, business_id, stars, date)

#Keep relevant user info
user_data_small <- user_data_small %>%
  select(user_id, review_count, yelping_since, elite)
  
##Transform date variables to dttm format
review_data_small$date <- parse_datetime(review_data_small$date, format = "%Y-%m-%d %H:%M:%S", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)
user_data_small$yelping_since <- parse_datetime(user_data_small$yelping_since, format = "%Y-%m-%d %H:%M:%S", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)

## merge review data with user data
review_data <- left_join(review_data_small, user_data_small, by = "user_id")

# Create new variable to indicate number of years a user has been yelping
review_data$years_yelping <- as.numeric(difftime(review_data$date, review_data$yelping_since, units = "weeks")) / 52.25

# Convert review date to year format
review_data$review_year <- as.numeric(format(as.Date(review_data$date), "%Y"))

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
review_data$is_elite <- mapply(is_elite, review_data$elite, review_data$review_year)

review_data <- review_data %>%
  select(-elite, -review_year)

# Calculate the average stars for each business_id for elite and non-elite users
avg_stars <- review_data %>%
  group_by(business_id, is_elite) %>%
  summarise(avg_stars = mean(stars, na.rm = TRUE)) %>%
  spread(is_elite, avg_stars)

# Calculate the difference in averages
avg_stars$diff <- avg_stars$"1" - avg_stars$"0"

# Join the difference back to the original data frame
review_data <- review_data %>%
  left_join(avg_stars %>% select(business_id, diff), by = "business_id")

#-------------------------------------------------------------------------------#

### COMBINE ALL DATA
master_data <- inner_join(review_data, business_data, by = "business_id")

master_data <- master_data[complete.cases(master_data), ]  #remove rows of missing x variables

save(master_data, file = "master_data.Rda")

#-------------------------------------------------------------------------------#

#Modelling 1: Linear Regression

# Copy new dataset for 1st specification
model1_data <- master_data


# Set seed for reproducibility
set.seed(1)

# Split the data into training and test sets
sample_size <- 10000
test_indices <- sample(1:nrow(model1_data), sample_size)
test_data <- model1_data[test_indices, ]
train_data <- model1_data[-test_indices, ]

# Specify the model formula
formula <- as.formula("stars.x ~ review_count.x + years_yelping + is_elite + diff*is_elite + city + state + postal_code + stars.y + review_count.y + wkend_open + rating_power + tip_count + checkin_freq + category")

### LINEAR MODEL
# Specify the control parameters for the train function
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Fit the model
mod_1 <- train(formula, data = train_data, method = "lm", trControl = ctrl)

# Print the summary of the model
summary(mod_1)

# Predict on the test data
predictions <- predict(mod_1, newdata = test_data)

# Calculate the mean squared error
mse <- postResample(pred = predictions, obs = test_data$stars.x)["MSE"]

print(paste("Mean Squared Error: ", mse))

#----------------------------------------#


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



