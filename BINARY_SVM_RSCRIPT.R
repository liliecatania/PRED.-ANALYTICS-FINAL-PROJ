###Load packages 
library(caret)
library(rpart.plot)
library(dplyr)
library(readr)
library(forcats)
library(tidyverse)

###Load the data 
flights_train<- read_csv("/Users/svc-olin130lounge/Downloads/flights_train.csv")
flights_test<- read_csv("/Users/svc-olin130lounge/Downloads/flights_test.csv")
flights_val<- read_csv("/Users/svc-olin130lounge/Downloads/flights_val.csv")


###Create Airline Ranking Based on Avg Airline Delay Column 
DELAY_RANK <- tibble::tibble(
  AIRLINE = c("DL", "HA", "EV", "UA", "AA", "OO", "B6", "MQ", "US", "WN", "AS", "F9", "NK", "VX"),
  DELAY_RANK = c(3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1)
)

###Ensure AIRLINE is character
flights_train$AIRLINE <- as.character(flights_train$AIRLINE)
flights_val$AIRLINE <- as.character(flights_val$AIRLINE)
flights_test$AIRLINE <- as.character(flights_test$AIRLINE)

###Join DELAY_RANK
flights_train <- flights_train %>% left_join(DELAY_RANK, by = "AIRLINE")
flights_val <- flights_val %>% left_join(DELAY_RANK, by = "AIRLINE")
flights_test <- flights_test %>% left_join(DELAY_RANK, by = "AIRLINE")

###Convert DELAY_RANK to factor
flights_train$DELAY_RANK <- as.factor(flights_train$DELAY_RANK)
flights_val$DELAY_RANK <- as.factor(flights_val$DELAY_RANK)
flights_test$DELAY_RANK <- as.factor(flights_test$DELAY_RANK)

###BINARY SCHEDULED DEPARTURE BINS
bin_departure_time <- function(df) {
  df %>%
    mutate(
      dep_hour = as.numeric(SCHEDULED_DEPARTURE) %/% 100,
      SCHEDULED_GRAVEYARD_DEPARTURE = ifelse(dep_hour >= 0 & dep_hour < 6, 1, 0),
      SCHEDULED_MORNING_DEPARTURE   = ifelse(dep_hour >= 6 & dep_hour < 12, 1, 0),
      SCHEDULED_AFTERNOON_DEPARTURE = ifelse(dep_hour >= 12 & dep_hour < 18, 1, 0),
      SCHEDULED_NIGHT_DEPARTURE     = ifelse(dep_hour >= 18 & dep_hour < 24, 1, 0)
    ) %>%
    select(-dep_hour)
}

flights_train <- bin_departure_time(flights_train)
flights_val <- bin_departure_time(flights_val)
flights_test <- bin_departure_time(flights_test)

###CHANGING THE DAY OF THE WEEK COLUMN TO BINARY CODED (CREATED 7 SEPEARTE COLS)
flights_train$DAY_OF_WEEK <- as.numeric(flights_train$DAY_OF_WEEK)

###Create dummy columns for each day
flights_train <- flights_train %>%
  mutate(
    MONDAY = ifelse(DAY_OF_WEEK == 1, 1, 0),
    TUESDAY = ifelse(DAY_OF_WEEK == 2, 1, 0),
    WEDNESDAY = ifelse(DAY_OF_WEEK == 3, 1, 0),
    THURSDAY = ifelse(DAY_OF_WEEK == 4, 1, 0),
    FRIDAY = ifelse(DAY_OF_WEEK == 5, 1, 0),
    SATURDAY = ifelse(DAY_OF_WEEK == 6, 1, 0),
    SUNDAY = ifelse(DAY_OF_WEEK == 7, 1, 0)
  )

flights_val <- flights_val %>%
  mutate(
    MONDAY = ifelse(DAY_OF_WEEK == 1, 1, 0),
    TUESDAY = ifelse(DAY_OF_WEEK == 2, 1, 0),
    WEDNESDAY = ifelse(DAY_OF_WEEK == 3, 1, 0),
    THURSDAY = ifelse(DAY_OF_WEEK == 4, 1, 0),
    FRIDAY = ifelse(DAY_OF_WEEK == 5, 1, 0),
    SATURDAY = ifelse(DAY_OF_WEEK == 6, 1, 0),
    SUNDAY = ifelse(DAY_OF_WEEK == 7, 1, 0)
  )

flights_test <- flights_test %>%
  mutate(
    MONDAY = ifelse(DAY_OF_WEEK == 1, 1, 0),
    TUESDAY = ifelse(DAY_OF_WEEK == 2, 1, 0),
    WEDNESDAY = ifelse(DAY_OF_WEEK == 3, 1, 0),
    THURSDAY = ifelse(DAY_OF_WEEK == 4, 1, 0),
    FRIDAY = ifelse(DAY_OF_WEEK == 5, 1, 0),
    SATURDAY = ifelse(DAY_OF_WEEK == 6, 1, 0),
    SUNDAY = ifelse(DAY_OF_WEEK == 7, 1, 0)
  )

###SCALING LATE_AIRCRAFT_DELAY
flights_train$LATE_AIRCRAFT_DELAY <- scale(flights_train$LATE_AIRCRAFT_DELAY)

flights_val$LATE_AIRCRAFT_DELAY <- (flights_val$LATE_AIRCRAFT_DELAY - attr(flights_train$LATE_AIRCRAFT_DELAY, "scaled:center")) / 
  attr(flights_train$LATE_AIRCRAFT_DELAY, "scaled:scale")

flights_test$LATE_AIRCRAFT_DELAY <- scale(flights_test$LATE_AIRCRAFT_DELAY)

###BINARY TARGET VARIABLE DEPARTURE_DELAY
flights_train$DEPARTURE_DELAY <- as.factor(ifelse(flights_train$DEPARTURE_DELAY > 0, 1, 0))
flights_val$DEPARTURE_DELAY <- as.factor(ifelse(flights_val$DEPARTURE_DELAY > 0, 1, 0))
flights_test$DEPARTURE_DELAY <- as.factor(ifelse(flights_test$DEPARTURE_DELAY > 0, 1, 0))

###SVM WITH RADIAL KERNEL (PRE-TUNE) 
library(e1071)
set.seed(123)
sample_idx <- sample(1:nrow(flights_train), 20000)
flights_train_small <- flights_train[sample_idx, ]

sample_idx2 <- sample(1:nrow(flights_val), 2000)
flights_val_small <- flights_val[sample_idx2, ]

###RAN MODEL USING SMALL TRAINING DATA 
SVM_Model <- svm(DEPARTURE_DELAY ~ 
                   SCHEDULED_MORNING_DEPARTURE + 
                   SCHEDULED_AFTERNOON_DEPARTURE + 
                   SCHEDULED_NIGHT_DEPARTURE + 
                   MONDAY + TUESDAY + WEDNESDAY + THURSDAY + 
                   FRIDAY + SATURDAY + SUNDAY + 
                   LATE_AIRCRAFT_DELAY + DELAY_RANK, 
                 data = flights_train_small, 
                 type = "C-classification",
                 kernel = "radial",
                 cost = 1,
                 gamma = 1 / (ncol(flights_train_small) - 1),
                 scale = FALSE)

print(SVM_Model)

###In-sample error (Training)
E_IN_SVM <- 1 - mean(predict(SVM_Model, flights_train) == flights_train$DEPARTURE_DELAY)
print(paste("In-sample error (E_IN_SVM):", round(E_IN_SVM, 4)))

###Out-of-sample error (Validation)
E_OUT_SVM <- 1 - mean(predict(SVM_Model, flights_val) == flights_val$DEPARTURE_DELAY)
print(paste("Out-of-sample error (E_OUT_SVM):", round(E_OUT_SVM, 4)))

###RE-RUNNING THE MODEL ON BALANCED DATASET 
###Ensure DEPARTURE_DELAY is numeric (0/1) 
flights_train$DEPARTURE_DELAY_NUM <- as.numeric(as.character(flights_train$DEPARTURE_DELAY))

###Check structure to confirm conversion
str(flights_train$DEPARTURE_DELAY_NUM)

# Separate the two classes
delayed_0 <- flights_train %>% filter(DEPARTURE_DELAY_NUM == 0)
delayed_1 <- flights_train %>% filter(DEPARTURE_DELAY_NUM == 1)

###Downsample the majority class (whichever is bigger)
set.seed(123)
if (nrow(delayed_0) > nrow(delayed_1)) {
  delayed_0_sample <- delayed_0 %>% sample_n(nrow(delayed_1))
  flights_train_bal <- bind_rows(delayed_0_sample, delayed_1)
} else {
  delayed_1_sample <- delayed_1 %>% sample_n(nrow(delayed_0))
  flights_train_bal <- bind_rows(delayed_0, delayed_1_sample)
}

###Check new balanced proportions
table(flights_train_bal$DEPARTURE_DELAY_NUM)
prop.table(table(flights_train_bal$DEPARTURE_DELAY_NUM))

###SVM WITH RADIAL KERNEL ON BALANCED DATA
library(e1071)
set.seed(123)

###Downsampled balanced training data (20,000 observations)
sample_idx_bal <- sample(1:nrow(flights_train_bal), 20000)
flights_train_bal_small <- flights_train_bal[sample_idx_bal, ]

###Validation sample (same as before, from imbalanced val set)
sample_idx2 <- sample(1:nrow(flights_val), 2000)
flights_val_small <- flights_val[sample_idx2, ]


###Train SVM on balanced, downsampled data
SVM_Model_Bal <- svm(DEPARTURE_DELAY ~ 
                       SCHEDULED_MORNING_DEPARTURE + 
                       SCHEDULED_AFTERNOON_DEPARTURE + 
                       SCHEDULED_NIGHT_DEPARTURE + 
                       MONDAY + TUESDAY + WEDNESDAY + THURSDAY + 
                       FRIDAY + SATURDAY + SUNDAY + 
                       LATE_AIRCRAFT_DELAY + DELAY_RANK, 
                     data = flights_train_bal_small, 
                     type = "C-classification",
                     kernel = "radial",
                     cost = 1,
                     gamma = 1 / (ncol(flights_train_bal_small) - 1),
                     scale = FALSE)

print(SVM_Model_Bal)

###In-sample error (Training on full balanced data)
E_IN_SVM_BAL <- 1 - mean(predict(SVM_Model_Bal, flights_train_bal) == flights_train_bal$DEPARTURE_DELAY)
print(paste("In-sample error (E_IN_SVM_BAL):", round(E_IN_SVM_BAL, 4)))

###Out-of-sample error (Validation set stays same imbalance)
E_OUT_SVM_BAL <- 1 - mean(predict(SVM_Model_Bal, flights_val) == flights_val$DEPARTURE_DELAY)
print(paste("Out-of-sample error (E_OUT_SVM_BAL):", round(E_OUT_SVM_BAL, 4)))

