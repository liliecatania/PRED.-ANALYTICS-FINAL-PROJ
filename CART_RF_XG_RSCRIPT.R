###Load Libraries 
library(tidyverse)
library(tidymodels)
library(caret)
library(rpart.plot)
library(yardstick)

###Load Data 
flights_train <- read_csv("~/Desktop/flights_train.csv")
flights_val   <- read_csv("~/Desktop/flights_val.csv")
flights_test  <- read_csv("~/Desktop/flights_test.csv")

###Airline Delay Ranks
DELAY_RANK <- tibble(
  AIRLINE = c("DL", "HA", "EV", "UA", "AA", "OO", "B6", "MQ", "US", "WN", "AS", "F9", "NK", "VX"),
  DELAY_RANK = c(3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1)
)

assign_delay_rank <- function(df) {
  df$AIRLINE <- as.character(df$AIRLINE)
  df %>%
    left_join(DELAY_RANK, by = "AIRLINE") %>%
    mutate(DELAY_RANK = as.factor(DELAY_RANK))
}

flights_train <- assign_delay_rank(flights_train)
flights_val   <- assign_delay_rank(flights_val)
flights_test  <- assign_delay_rank(flights_test)

###Departure Time Binning
bin_departure_time <- function(df) {
  df %>%
    mutate(
      dep_hour = as.numeric(SCHEDULED_DEPARTURE) %/% 100,
      SCHEDULED_GRAVEYARD_DEPARTURE = ifelse(dep_hour >= 0  & dep_hour < 6, 1, 0),
      SCHEDULED_MORNING_DEPARTURE   = ifelse(dep_hour >= 6  & dep_hour < 12, 1, 0),
      SCHEDULED_AFTERNOON_DEPARTURE = ifelse(dep_hour >= 12 & dep_hour < 18, 1, 0),
      SCHEDULED_NIGHT_DEPARTURE     = ifelse(dep_hour >= 18 & dep_hour < 24, 1, 0)
    ) %>%
    select(-dep_hour)
}

flights_train <- bin_departure_time(flights_train)
flights_val   <- bin_departure_time(flights_val)
flights_test  <- bin_departure_time(flights_test)

###Day of Week Dummy Variables
create_day_dummies <- function(df) {
  df %>%
    mutate(
      MONDAY    = ifelse(DAY_OF_WEEK == 1, 1, 0),
      TUESDAY   = ifelse(DAY_OF_WEEK == 2, 1, 0),
      WEDNESDAY = ifelse(DAY_OF_WEEK == 3, 1, 0),
      THURSDAY  = ifelse(DAY_OF_WEEK == 4, 1, 0),
      FRIDAY    = ifelse(DAY_OF_WEEK == 5, 1, 0),
      SATURDAY  = ifelse(DAY_OF_WEEK == 6, 1, 0),
      SUNDAY    = ifelse(DAY_OF_WEEK == 7, 1, 0)
    )
}

flights_train <- create_day_dummies(flights_train)
flights_val   <- create_day_dummies(flights_val)
flights_test  <- create_day_dummies(flights_test)

###Categorize Departure Delay
binarize_departure_delay_multiclass <- function(df, delay_col = "DEPARTURE_DELAY") {
  df %>%
    mutate(
      DEPARTURE_DELAY_NUMERIC = as.numeric(as.character(!!sym(delay_col))),
      DEPARTURE_DELAY_CAT = case_when(
        DEPARTURE_DELAY_NUMERIC == 0 ~ "OnTime",
        DEPARTURE_DELAY_NUMERIC > 0 & DEPARTURE_DELAY_NUMERIC <= 15 ~ "Short",
        DEPARTURE_DELAY_NUMERIC > 15 & DEPARTURE_DELAY_NUMERIC <= 60 ~ "Medium",
        DEPARTURE_DELAY_NUMERIC > 60 ~ "Long"
      ),
      DEPARTURE_DELAY_CAT = factor(DEPARTURE_DELAY_CAT, levels = c("OnTime", "Short", "Medium", "Long"))
    ) %>%
    select(-DEPARTURE_DELAY_NUMERIC)
}

flights_train <- binarize_departure_delay_multiclass(flights_train)
flights_val   <- binarize_departure_delay_multiclass(flights_val)
flights_test  <- binarize_departure_delay_multiclass(flights_test)

###Time of Day Factor Column
assign_time_block <- function(df) {
  df %>%
    mutate(SCHEDULED_DEP_BIN = case_when(
      SCHEDULED_GRAVEYARD_DEPARTURE == 1 ~ "Graveyard",
      SCHEDULED_MORNING_DEPARTURE   == 1 ~ "Morning",
      SCHEDULED_AFTERNOON_DEPARTURE == 1 ~ "Afternoon",
      SCHEDULED_NIGHT_DEPARTURE     == 1 ~ "Night"
    )) %>%
    mutate(SCHEDULED_DEP_BIN = factor(SCHEDULED_DEP_BIN, levels = c("Graveyard", "Morning", "Afternoon", "Night")))
}

flights_train <- assign_time_block(flights_train)
flights_val   <- assign_time_block(flights_val)
flights_test   <- assign_time_block(flights_test)

###Convert DAY_OF_WEEK to Factor
flights_train$DAY_OF_WEEK <- as.factor(flights_train$DAY_OF_WEEK)
flights_val$DAY_OF_WEEK   <- as.factor(flights_val$DAY_OF_WEEK)
flights_test$DAY_OF_WEEK   <- as.factor(flights_test$DAY_OF_WEEK)

###CART Model Specification
cart_spec <- decision_tree(
  cost_complexity = 0.00001,
  tree_depth = 30,
  min_n = 5
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

###CART Model Fitting
cart_fit <- cart_spec %>%
  fit(
    DEPARTURE_DELAY_CAT ~ 
      DAY_OF_WEEK + 
      SCHEDULED_DEP_BIN + 
      LATE_AIRCRAFT_DELAY + 
      DELAY_RANK,
    data = flights_train
  )

###CART Tree Visualization
custom_colors <- list(
  "lightgreen",  # Short
  "orange",      # Medium
  "red",         # Long
  "green"        # OnTime
)

rpart.plot(
  cart_fit$fit,
  type = 4,
  extra = 2,
  roundint = FALSE,
  fallen.leaves = TRUE,
  box.palette = custom_colors
)

###Training Predictions & Evaluation
# Class predictions on training set
train_preds <- predict(cart_fit, new_data = flights_train, type = "class") %>%
  pull(.pred_class)  # <-- this fixes the issue!

# Add to training data
train_results <- flights_train %>%
  mutate(.pred_class = train_preds)

# Confusion matrix
train_conf_mat <- table(Predicted = train_results$.pred_class,
                        Actual = train_results$DEPARTURE_DELAY_CAT)

train_metrics <- confusionMatrix(train_conf_mat)
print(train_metrics)

###Validation Predictions & Evaluation
val_preds <- predict(cart_fit, new_data = flights_val, type = "class") %>%
  pull(.pred_class)

val_results <- flights_val %>%
  mutate(.pred_class = val_preds)

val_conf_mat <- table(Predicted = val_results$.pred_class,
                      Actual = val_results$DEPARTURE_DELAY_CAT)

val_metrics <- confusionMatrix(val_conf_mat)
print(val_metrics)

###In-sample (training) error rate
train_error <- 1 - train_metrics$overall["Accuracy"]
print(train_error)

###Out-of-sample (validation) error rate
val_error <- 1 - val_metrics$overall["Accuracy"]
print(val_error)

###Balancing Multiclass Output: Count how many examples per class
table(flights_train$DEPARTURE_DELAY_CAT)

###Find the smallest class size
min_class_size <- flights_train %>%
  count(DEPARTURE_DELAY_CAT) %>%
  summarise(min(n)) %>%
  pull()

###Downsample each class to match the smallest one
set.seed(123)  # for reproducibility
flights_train_bal <- flights_train %>%
  group_by(DEPARTURE_DELAY_CAT) %>%
  sample_n(min_class_size) %>%
  ungroup()

###CART Model on Balanced Training Set
cart_fit_bal <- cart_spec %>%
  fit(
    DEPARTURE_DELAY_CAT ~ 
      DAY_OF_WEEK + 
      SCHEDULED_DEP_BIN + 
      LATE_AIRCRAFT_DELAY + 
      DELAY_RANK,
    data = flights_train_bal
  )

###In-Sample Predictions & Evaluation (Balanced)
train_preds_bal <- predict(cart_fit_bal, new_data = flights_train_bal, type = "class") %>%
  pull(.pred_class)

train_results_bal <- flights_train_bal %>%
  mutate(.pred_class = train_preds_bal)

train_conf_mat_bal <- table(Predicted = train_results_bal$.pred_class,
                            Actual = train_results_bal$DEPARTURE_DELAY_CAT)

train_metrics_bal <- confusionMatrix(train_conf_mat_bal)
print(train_metrics_bal)

###Out-of-Sample Predictions & Evaluation (Validation)
val_preds_bal <- predict(cart_fit_bal, new_data = flights_val, type = "class") %>%
  pull(.pred_class)

val_results_bal <- flights_val %>%
  mutate(.pred_class = val_preds_bal)

val_conf_mat_bal <- table(Predicted = val_results_bal$.pred_class,
                          Actual = val_results_bal$DEPARTURE_DELAY_CAT)

val_metrics_bal <- confusionMatrix(val_conf_mat_bal)
print(val_metrics_bal)

###Error Rate Summary
train_error_bal <- 1 - train_metrics_bal$overall["Accuracy"]
print(train_error_bal)

val_error_bal   <- 1 - val_metrics_bal$overall["Accuracy"]
print(val_error_bal)

###Prune CART Using 1-SE Rule + Visualize
# Extract cp table from original CART model
plotcp(cart_fit$fit)
cp_table <- printcp(cart_fit$fit)

###Get minimum xerror and its standard error
min_xerror <- min(cp_table[,"xerror"])
se <- cp_table[which.min(cp_table[,"xerror"]), "xstd"]

###Find largest cp where xerror is within 1 SE of the minimum
cp_1se <- cp_table %>%
  as.data.frame() %>%
  filter(xerror <= min_xerror + se) %>%
  arrange(desc(CP)) %>%
  slice(1) %>%
  pull(CP)

cat("Simpler CP selected using 1-SE rule:", cp_1se, "\n")

###Prune the tree using the 1-SE cp
pruned_tree <- rpart::prune(cart_fit$fit, cp = cp_1se)

###Custom color palette (ensure order matches pruned_tree$ylevels)
custom_colors <- c("green", "lightgreen", "orange", "red")

###Visualize pruned tree
rpart.plot(
  pruned_tree,
  type = 4,
  extra = 2,
  roundint = FALSE,
  fallen.leaves = TRUE,
  box.col = custom_colors[pruned_tree$frame$yval]
)

###In-Sample Predictions & Error
train_preds_pruned <- predict(pruned_tree, newdata = flights_train, type = "class")

train_results_pruned <- flights_train %>%
  mutate(.pred_class = train_preds_pruned)

train_conf_mat_pruned <- table(Predicted = train_results_pruned$.pred_class,
                               Actual = train_results_pruned$DEPARTURE_DELAY_CAT)

train_metrics_pruned <- confusionMatrix(train_conf_mat_pruned)
E_IN_PRUNED <- 1 - train_metrics_pruned$overall["Accuracy"]
print(E_IN_PRUNED)

###Out-of-Sample Predictions & Error
val_preds_pruned <- predict(pruned_tree, newdata = flights_val, type = "class")

val_results_pruned <- flights_val %>%
  mutate(.pred_class = val_preds_pruned)

val_conf_mat_pruned <- table(Predicted = val_results_pruned$.pred_class,
                             Actual = val_results_pruned$DEPARTURE_DELAY_CAT)

val_metrics_pruned <- confusionMatrix(val_conf_mat_pruned)
E_OUT_PRUNED <- 1 - val_metrics_pruned$overall["Accuracy"]
print(E_OUT_PRUNED)

###Validation Confusion Matrix: Pruned Tree
# Predict classes on validation set using pruned model
val_preds_pruned <- predict(pruned_tree, newdata = flights_val, type = "class")

###Create labeled results data frame
val_results_pruned <- flights_val %>%
  mutate(.pred_class = val_preds_pruned)

###Create confusion matrix table
val_conf_mat_pruned <- table(Predicted = val_results_pruned$.pred_class,
                             Actual = val_results_pruned$DEPARTURE_DELAY_CAT)

###Output full confusion matrix & class metrics
val_metrics_pruned <- confusionMatrix(val_conf_mat_pruned)
print(val_metrics_pruned)

###CART on Balanced Data: Fit, Prune, Visualize, Evaluate 
###Fit CART on balanced training data
set.seed(123)
cart_bal_fit <- decision_tree(
  cost_complexity = 0.00001,
  tree_depth = 30,
  min_n = 5
) %>%
  set_engine("rpart") %>%
  set_mode("classification") %>%
  fit(DEPARTURE_DELAY_CAT ~ 
        DAY_OF_WEEK + 
        SCHEDULED_DEP_BIN + 
        LATE_AIRCRAFT_DELAY + 
        DELAY_RANK,
      data = flights_train_bal)

###Prune using 1-SE rule
cp_table_bal <- printcp(cart_bal_fit$fit)
min_xerror_bal <- min(cp_table_bal[,"xerror"])
se_bal <- cp_table_bal[which.min(cp_table_bal[,"xerror"]), "xstd"]

cp_1se_bal <- cp_table_bal %>%
  as.data.frame() %>%
  filter(xerror <= min_xerror_bal + se_bal) %>%
  arrange(desc(CP)) %>%
  slice(1) %>%
  pull(CP)

cat("1-SE CP from balanced model:", cp_1se_bal, "\n")

pruned_tree_bal <- rpart::prune(cart_bal_fit$fit, cp = cp_1se_bal)

###Visualize the pruned tree
custom_colors <- c("green", "lightgreen", "orange", "red")

rpart.plot(
  pruned_tree_bal,
  type = 4,
  extra = 2,
  roundint = FALSE,
  fallen.leaves = TRUE,
  box.col = custom_colors[pruned_tree_bal$frame$yval]
)

###In-Sample Evaluation (Training)
train_preds_bal_pruned <- predict(pruned_tree_bal, newdata = flights_train_bal, type = "class")

train_results_bal_pruned <- flights_train_bal %>%
  mutate(.pred_class = train_preds_bal_pruned)

train_conf_mat_bal_pruned <- table(Predicted = train_results_bal_pruned$.pred_class,
                                   Actual = train_results_bal_pruned$DEPARTURE_DELAY_CAT)

train_metrics_bal_pruned <- confusionMatrix(train_conf_mat_bal_pruned)
E_IN_BAL_PRUNED <- 1 - train_metrics_bal_pruned$overall["Accuracy"]
print(E_IN_BAL_PRUNED)

###Print training confusion matrix
print(train_metrics_bal_pruned)

###Out-of-Sample Evaluation (Validation)
val_preds_bal_pruned <- predict(pruned_tree_bal, newdata = flights_val, type = "class")

val_results_bal_pruned <- flights_val %>%
  mutate(.pred_class = val_preds_bal_pruned)

val_conf_mat_bal_pruned <- table(Predicted = val_results_bal_pruned$.pred_class,
                                 Actual = val_results_bal_pruned$DEPARTURE_DELAY_CAT)

val_metrics_bal_pruned <- confusionMatrix(val_conf_mat_bal_pruned)
E_OUT_BAL_PRUNED <- 1 - val_metrics_bal_pruned$overall["Accuracy"]
print(E_OUT_BAL_PRUNED)

###Print validation confusion matrix
print(val_metrics_bal_pruned)

###Load Libraries 
library(tidymodels)
library(ranger)
library(caret)
library(vip)

###Define Model Formula
fmla <- DEPARTURE_DELAY_CAT ~ 
  LATE_AIRCRAFT_DELAY + 
  DELAY_RANK + 
  DAY_OF_WEEK + 
  SCHEDULED_DEP_BIN

###Specify Random Forest Model
spec_rf <- rand_forest(
  min_n = 20,       # Minimum observations required to split
  trees = 100,      # Number of trees in the ensemble
  mtry = 3          # Number of predictors randomly selected at each split
) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")

###Fit Model on Imbalanced Training Data
set.seed(123)
random_forest <- spec_rf %>%
  fit(formula = fmla, data = flights_train)

###Print Model Summary
print(random_forest)

###Evaluate Random Forest Model
#In-Sample Predictions (Training)
train_preds_rf <- predict(random_forest, new_data = flights_train, type = "class") %>%
  pull(.pred_class)

train_results_rf <- flights_train %>%
  mutate(.pred_class = train_preds_rf)

train_conf_mat_rf <- table(Predicted = train_results_rf$.pred_class,
                           Actual = train_results_rf$DEPARTURE_DELAY_CAT)

train_metrics_rf <- confusionMatrix(train_conf_mat_rf)
E_IN_RF <- 1 - train_metrics_rf$overall["Accuracy"]
print(E_IN_RF)

###Print training confusion matrix
print(train_metrics_rf)

###Out-of-Sample Predictions (Validation)
val_preds_rf <- predict(random_forest, new_data = flights_val, type = "class") %>%
  pull(.pred_class)

val_results_rf <- flights_val %>%
  mutate(.pred_class = val_preds_rf)

val_conf_mat_rf <- table(Predicted = val_results_rf$.pred_class,
                         Actual = val_results_rf$DEPARTURE_DELAY_CAT)

val_metrics_rf <- confusionMatrix(val_conf_mat_rf)
E_OUT_RF <- 1 - val_metrics_rf$overall["Accuracy"]
print(E_OUT_RF)

###Print validation confusion matrix
print(val_metrics_rf)

###Random Forest on Balanced Training Set: 
###Define Model Formula
fmla <- DEPARTURE_DELAY_CAT ~ 
  LATE_AIRCRAFT_DELAY + 
  DELAY_RANK + 
  DAY_OF_WEEK + 
  SCHEDULED_DEP_BIN

###Specify Random Forest Model
spec_rf_bal <- rand_forest(
  min_n = 20,
  trees = 100,
  mtry = 3
) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")

###Fit Model on Balanced Training Data
set.seed(123)
random_forest_bal <- spec_rf_bal %>%
  fit(formula = fmla, data = flights_train_bal)

###Evaluate Random Forest Model

###In-Sample Predictions (Training)
train_preds_rf_bal <- predict(random_forest_bal, new_data = flights_train_bal, type = "class") %>%
  pull(.pred_class)

train_results_rf_bal <- flights_train_bal %>%
  mutate(.pred_class = train_preds_rf_bal)

train_conf_mat_rf_bal <- table(Predicted = train_results_rf_bal$.pred_class,
                               Actual = train_results_rf_bal$DEPARTURE_DELAY_CAT)

train_metrics_rf_bal <- confusionMatrix(train_conf_mat_rf_bal)
E_IN_RF_BAL <- 1 - train_metrics_rf_bal$overall["Accuracy"]
print(E_IN_RF_BAL)

###Print training confusion matrix
print(train_metrics_rf_bal)

###Out-of-Sample Predictions (Validation)
val_preds_rf_bal <- predict(random_forest_bal, new_data = flights_val, type = "class") %>%
  pull(.pred_class)

val_results_rf_bal <- flights_val %>%
  mutate(.pred_class = val_preds_rf_bal)

val_conf_mat_rf_bal <- table(Predicted = val_results_rf_bal$.pred_class,
                             Actual = val_results_rf_bal$DEPARTURE_DELAY_CAT)

val_metrics_rf_bal <- confusionMatrix(val_conf_mat_rf_bal)
E_OUT_RF_BAL <- 1 - val_metrics_rf_bal$overall["Accuracy"]
print(E_OUT_RF_BAL)

###Print validation confusion matrix
print(val_metrics_rf_bal)

###XGBoost Model
#Load Libraries
library(xgboost)

###Define Model Formula
fmla <- DEPARTURE_DELAY_CAT ~ 
  LATE_AIRCRAFT_DELAY + 
  DELAY_RANK + 
  DAY_OF_WEEK + 
  SCHEDULED_DEP_BIN

###Specify XGBoost Model
spec_xb <- boost_tree(
  min_n = 20,
  tree_depth = 6,
  trees = 100,
  mtry = 3,
  sample_size = 1.0,
  learn_rate = 0.1,
  loss_reduction = 0,
  stop_iter = NULL
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

###Fit Model on Imbalanced Training Data
set.seed(123)
boosted_forest <- spec_xb %>%
  fit(formula = fmla, data = flights_train)
print(boosted_forest)

###Evaluate XGBoost Model

###In-Sample Predictions (Training)
train_preds_xgb <- predict(boosted_forest, new_data = flights_train, type = "class") %>%
  pull(.pred_class)

train_results_xgb <- flights_train %>%
  mutate(.pred_class = train_preds_xgb)

train_conf_mat_xgb <- table(Predicted = train_results_xgb$.pred_class,
                            Actual = train_results_xgb$DEPARTURE_DELAY_CAT)

train_metrics_xgb <- confusionMatrix(train_conf_mat_xgb)
E_IN_XGB <- 1 - train_metrics_xgb$overall["Accuracy"]
print(E_IN_XGB)

print(train_metrics_xgb)

###Out-of-Sample Predictions (Validation)
val_preds_xgb <- predict(boosted_forest, new_data = flights_val, type = "class") %>%
  pull(.pred_class)

val_results_xgb <- flights_val %>%
  mutate(.pred_class = val_preds_xgb)

val_conf_mat_xgb <- table(Predicted = val_results_xgb$.pred_class,
                          Actual = val_results_xgb$DEPARTURE_DELAY_CAT)

val_metrics_xgb <- confusionMatrix(val_conf_mat_xgb)
E_OUT_XGB <- 1 - val_metrics_xgb$overall["Accuracy"]
print(E_OUT_XGB)

print(val_metrics_xgb)

###Define Model Formula
fmla <- DEPARTURE_DELAY_CAT ~ 
  LATE_AIRCRAFT_DELAY + 
  DELAY_RANK + 
  DAY_OF_WEEK + 
  SCHEDULED_DEP_BIN

###C. Specify XGBoost Model
spec_xb_bal <- boost_tree(
  min_n = 20,
  tree_depth = 6,
  trees = 100,
  mtry = 3,
  sample_size = 1.0,
  learn_rate = 0.1,
  loss_reduction = 0,
  stop_iter = NULL
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

###Fit Model on Balanced Training Data
set.seed(123)
boosted_forest_bal <- spec_xb_bal %>%
  fit(formula = fmla, data = flights_train_bal)

###Evaluate XGBoost Model

# In-Sample Predictions (Training)
train_preds_xgb_bal <- predict(boosted_forest_bal, new_data = flights_train_bal, type = "class") %>%
  pull(.pred_class)

train_results_xgb_bal <- flights_train_bal %>%
  mutate(.pred_class = train_preds_xgb_bal)

train_conf_mat_xgb_bal <- table(Predicted = train_results_xgb_bal$.pred_class,
                                Actual = train_results_xgb_bal$DEPARTURE_DELAY_CAT)

train_metrics_xgb_bal <- confusionMatrix(train_conf_mat_xgb_bal)
E_IN_XGB_BAL <- 1 - train_metrics_xgb_bal$overall["Accuracy"]
print(E_IN_XGB_BAL)

print(train_metrics_xgb_bal)

# Out-of-Sample Predictions (Validation)
val_preds_xgb_bal <- predict(boosted_forest_bal, new_data = flights_val, type = "class") %>%
  pull(.pred_class)

val_results_xgb_bal <- flights_val %>%
  mutate(.pred_class = val_preds_xgb_bal)

val_conf_mat_xgb_bal <- table(Predicted = val_results_xgb_bal$.pred_class,
                              Actual = val_results_xgb_bal$DEPARTURE_DELAY_CAT)

val_metrics_xgb_bal <- confusionMatrix(val_conf_mat_xgb_bal)
E_OUT_XGB_BAL <- 1 - val_metrics_xgb_bal$overall["Accuracy"]
print(E_OUT_XGB_BAL)

print(val_metrics_xgb_bal)

### ─── Final XGBoost Model Evaluation (Uncontaminated Test Data) ──
#Predict on test set using best model
test_preds_xgb <- predict(boosted_forest, new_data = flights_test, type = "class") %>%
  pull(.pred_class)

###Combine predictions with test labels
test_results_xgb <- flights_test %>%
  mutate(.pred_class = test_preds_xgb)

###Generate confusion matrix
test_conf_mat_xgb <- table(Predicted = test_results_xgb$.pred_class,
                           Actual = test_results_xgb$DEPARTURE_DELAY_CAT)

test_metrics_xgb <- confusionMatrix(test_conf_mat_xgb)

###Calculate final test error
E_TEST_XGB <- 1 - test_metrics_xgb$overall["Accuracy"]
print(E_TEST_XGB)

###Report results
print(test_metrics_xgb)
