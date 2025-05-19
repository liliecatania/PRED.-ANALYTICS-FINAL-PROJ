
######################
####### FINAL ########
###### VERSION #######
######################

#load data sets always
flights_train<- read_csv("~/Desktop/flights_train.csv")
flights_test<- read_csv("~/Desktop/flights_test.csv")
flights_val<- read_csv("~/Desktop/flights_val.csv")

######################################################################################################
#2 hour delay binary  
flights_train$DELAYED_2 <- ifelse(flights_train$DEPARTURE_DELAY > 120 ,1, 0)
flights_test$DELAYED_2 <- ifelse(flights_test$DEPARTURE_DELAY > 120 ,1, 0)


sum(flights_train$DELAYED_2 == 1, na.rm = TRUE)

# Model with the Delays as predictors 
log_model <- glm(DELAYED_2 ~ WEATHER_DELAY + LATE_AIRCRAFT_DELAY + AIRLINE_DELAY + AIR_SYSTEM_DELAY,
                 data = flights_train, family = "binomial")

# Predict probabilities
pred_prob_in <- predict(log_model, type = "response")

# Predict class: 1 if prob > 0.5
pred_class_in <- ifelse(pred_prob_in > 0.5, 1, 0)

# Accuracy = correct predictions / total
mean(pred_class_in == flights_train$DELAYED_2)

library(pROC)
library(caret)
library(tidymodels)

# ROC and plot 
roc_obj <- roc(flights_train$DELAYED_2, pred_prob_in)
plot(roc_obj, main = "ROC Curve (In-Sample)")

#AUC
auc(roc_obj)

#this model had tooo much accuracy
###############################################################################################
##############################################################################################################
#stepwise fxn NUMERO DOS
# Specify a null model with no predictors
null_model2 <- glm(DELAYED_2 ~ 1, data = flights_test, family = "binomial")

# Specify the full model using all of the potential predictors
full_model2 <- glm(DELAYED_2 ~ DAY_OF_WEEK + TAXI_OUT + DISTANCE + SCHEDULED_GRAVEYARD_DEPARTURE +
                     SCHEDULED_MORNING_DEPARTURE + SCHEDULED_AFTERNOON_DEPARTURE + SCHEDULED_NIGHT_DEPARTURE
                   + WEATHER_DELAY + LATE_AIRCRAFT_DELAY + AIRLINE_DELAY + AIR_SYSTEM_DELAY + SECURITY_DELAY,
                   data = flights_train, family = "binomial", control = glm.control(maxit = 50))

# Use a forward stepwise algorithm to build a parsimonious model
step_model2 <- stats::step(null_model2, scope = list(lower = null_model2, upper = full_model2), direction = "forward")

# Estimate the stepwise donation probability
step_prob2 <- predict(step_model2, type = "response")

# Plot the ROC of the stepwise model
library(pROC)
ROC <- roc(flights_test$DELAYED_2, step_prob2)
plot(ROC, col = "red")
auc(ROC)

# ideal2: DELAYED_2 ~ LATE_AIRCRAFT_DELAY + AIRLINE_DELAY + AIR_SYSTEM_DELAY + 
#WEATHER_DELAY + TAXI_OUT + SECURITY_DELAY + DISTANCE + SCHEDULED_NIGHT_DEPARTURE + 
#  SCHEDULED_GRAVEYARD_DEPARTURE


##############################################################################################################


#START OVER load data sets again
flights_train<- read_csv("~/Desktop/flights_train.csv")
flights_test<- read_csv("~/Desktop/flights_test.csv")
flights_val<- read_csv("~/Desktop/flights_val.csv")


# making departure delay (binary)
#train
flights_train$DEPARTURE_DELAY <- as.numeric(as.character(flights_train$DEPARTURE_DELAY))
flights_train$DELAYED <- ifelse(flights_train$DEPARTURE_DELAY > 0, 1, 0)

#val 
flights_val$DEPARTURE_DELAY <- as.numeric(as.character(flights_val$DEPARTURE_DELAY))
flights_val$DELAYED <- ifelse(flights_val$DEPARTURE_DELAY > 0, 1, 0)

#test
flights_test$DEPARTURE_DELAY <- as.numeric(as.character(flights_test$DEPARTURE_DELAY))
flights_test$DELAYED <- ifelse(flights_test$DEPARTURE_DELAY > 0, 1, 0)

library(dplyr)
library(tidyverse)  
############################################
#### INPUT VARIABLE #1: AIRLINE_RANK ####### 
############################################
# Define delay ranks
DELAY_RANK <- tibble::tibble(
  AIRLINE = c("DL", "HA", "EV", "UA", "AA", "OO", "B6", "MQ", "US", "WN", "AS", "F9", "NK", "VX"),
  DELAY_RANK = c(3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1)
)

# Join with flights data
flights_train <- flights_train %>%
  left_join(DELAY_RANK, by = "AIRLINE")

flights_val <- flights_val %>%
  left_join(DELAY_RANK, by = "AIRLINE")

flights_test <- flights_test %>%
  left_join(DELAY_RANK, by = "AIRLINE")

# Converts Delay_Rank to factor for modeling
flights_train$CAT_DELAY_RANK <- as.factor(flights_train$DELAY_RANK)
flights_val$CAT_DELAY_RANK <- as.factor(flights_val$DELAY_RANK)
flights_test$CAT_DELAY_RANK <- as.factor(flights_test$DELAY_RANK)

# Check how many Tier 1 airlines are present
sum(flights_train$DELAY_RANK == 1)

############################################
#### INPUT VARIABLE #2: DAY_OF_WEEK ####### 
############################################


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


############################################
##### INPUT VARIABLE #3: DEPARTURE TIME#####
############################################
# One-hot encode scheduled departure time into binary columns
bin_departure_time <- function(df) {
  df %>%
    mutate(
      dep_hour = as.numeric(SCHEDULED_DEPARTURE) %/% 100,
      SCHEDULED_GRAVEYARD_DEPARTURE = ifelse(dep_hour >= 0 & dep_hour < 6, 1, 0),
      SCHEDULED_MORNING_DEPARTURE   = ifelse(dep_hour >= 6 & dep_hour < 12, 1, 0),
      SCHEDULED_AFTERNOON_DEPARTURE = ifelse(dep_hour >= 12 & dep_hour < 18, 1, 0),
      SCHEDULED_NIGHT_DEPARTURE     = ifelse(dep_hour >= 18 & dep_hour < 24, 1, 0)
    )
}


# Apply function to data sets
flights_train <- bin_departure_time(flights_train)
flights_test <- bin_departure_time(flights_test)
flights_val <- bin_departure_time(flights_val)

############################################
####### OUTPUT VARIABLE: BINARY ############
############################################

# Check original counts
table(flights_train$DELAYED)
prop.table(table(flights_train$DELAYED))

#for slides ########################################################################################
class_counts <- table(flights_train$DELAYED)
class_props <- prop.table(class_counts)

# Convert to data frame
df <- data.frame(
  Class = c("Not Delayed (0)", "Delayed (1)"),
  Count = as.numeric(class_counts),
  Proportion = as.numeric(class_props)
)

# Plot
library(ggplot2)

ggplot(df, aes(x = Class, y = Count, fill = Class)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.01)), 
            vjust = -0.5, size = 5) +
  labs(title = "Class Imbalance in Training Data",
       y = "Number of Observations",
       x = "") +  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none")

# NOT BALANCED #

model1 <- glm(DELAYED ~ MONDAY + TUESDAY + THURSDAY + FRIDAY +
                SCHEDULED_GRAVEYARD_DEPARTURE +
                SCHEDULED_MORNING_DEPARTURE +
                SCHEDULED_AFTERNOON_DEPARTURE +
                CAT_DELAY_RANK,
    data = flights_train, family = "binomial")

summary(model1)

########################################################################################
# Ensure DELAYED is numeric (0/1)
# Converts factor to numeric properly (avoids level-numbering trap)
flights_train$DELAYED <- as.numeric(as.character(flights_train$DELAYED))

# Separates the two classes
delayed_0 <- flights_train %>% filter(DELAYED == 0)
delayed_1 <- flights_train %>% filter(DELAYED == 1)

# Down-samples the majority class (delayed == 1)
set.seed(123)
delayed_1_sample <- delayed_1 %>% sample_n(nrow(delayed_0))

# Combines into balanced training set
flights_train_bal <- bind_rows(delayed_0, delayed_1_sample)

# MODEL BALANCED VERSION 
table(flights_train_bal$DELAYED)
prop.table(table(flights_train_bal$DELAYED))

#FOR VISUALS FOR SLIDES ####################################################################
class_counts_b <- table(flights_train_bal$DELAYED)
class_props_b <- prop.table(class_counts_b)

# Convert to data frame
df_b <- data.frame(
  Class = c("Not Delayed (0)", "Delayed (1)"),
  Count = as.numeric(class_counts_b),
  Proportion = as.numeric(class_props_b)
)

# Plot
# Pie chart from balanced data
ggplot(df_b, aes(x = "", y = Count, fill = Class)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.01)), 
            position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Balanced Class Distribution (Pie Chart)",
       x = NULL,
       y = NULL,
       fill = "Class") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))

############################################################################################
logit_balanced <- glm(DELAYED ~ MONDAY + TUESDAY + WEDNESDAY + THURSDAY + FRIDAY + SATURDAY + SUNDAY +
                        SCHEDULED_GRAVEYARD_DEPARTURE + 
                        SCHEDULED_MORNING_DEPARTURE + 
                        SCHEDULED_AFTERNOON_DEPARTURE + 
                        CAT_DELAY_RANK,
                      data = flights_train_bal, family = "binomial")

summary(logit_balanced)

# REFINED BALANCED MODEL
logit_model <- glm(DELAYED ~ MONDAY + TUESDAY + THURSDAY + FRIDAY +
                                SCHEDULED_GRAVEYARD_DEPARTURE +
                                SCHEDULED_MORNING_DEPARTURE +
                                SCHEDULED_AFTERNOON_DEPARTURE +
                                CAT_DELAY_RANK,
                              data = flights_train_bal, family = "binomial")

summary(logit_model)


# PROBIT ON BALANCED REFINED 
probit_model <- glm(DELAYED ~ MONDAY + TUESDAY + THURSDAY + FRIDAY +
                      SCHEDULED_GRAVEYARD_DEPARTURE +
                      SCHEDULED_MORNING_DEPARTURE +
                      SCHEDULED_AFTERNOON_DEPARTURE +
                      CAT_DELAY_RANK,
                    data = flights_train_bal, family = binomial("probit"))

summary(probit_model)

# MAKING PREDICTIONS 
pred_prob_logit <- predict(logit_model, type = "response")
pred_prob_probit <- predict(probit_model, type = "response")

# Predicted  (threshold = 0.5)
pred_class_logit <- ifelse(pred_prob_logit > 0.5, 1, 0)
pred_class_probit <- ifelse(pred_prob_probit > 0.5, 1, 0)

# Accuracy
logit_acc <- mean(pred_class_logit == flights_train_bal$DELAYED)
probit_acc <- mean(pred_class_probit == flights_train_bal$DELAYED)

logit_acc
probit_acc

table(Logit = pred_class_logit, Actual = flights_train_bal$DELAYED)
table(Probit = pred_class_probit, Actual = flights_train_bal$DELAYED)

#ROC CURVES
library(pROC)

# Logit ROC
roc_logit <- roc(flights_train_bal$DELAYED, pred_prob_logit)
auc_logit <- auc(roc_logit)

# Probit ROC
roc_probit <- roc(flights_train_bal$DELAYED, pred_prob_probit)
auc_probit <- auc(roc_probit)

# Plot both ROC curves
plot(roc_logit, col = "blue", main = "ROC Curve: Logit vs Probit")
lines(roc_probit, col = "red")
legend("bottomright", legend = c("Logit", "Probit"),
       col = c("blue", "red"), lwd = 2)


# OUT OF SAMPLE ACCURACY
flights_val$DELAYED <- as.numeric(as.character(flights_val$DELAYED))

# Logit predictions
pred_prob_logit_val <- predict(logit_model, newdata = flights_val, type = "response")
pred_class_logit_val <- ifelse(pred_prob_logit_val > 0.5, 1, 0)

# Probit predictions
pred_prob_probit_val <- predict(probit_model, newdata = flights_val, type = "response")
pred_class_probit_val <- ifelse(pred_prob_probit_val > 0.5, 1, 0)

# Accuracy
mean(pred_class_logit_val == flights_val$DELAYED)
mean(pred_class_probit_val == flights_val$DELAYED)

# Confusion matrices
table(Logit = pred_class_logit_val, Actual = flights_val$DELAYED)
table(Probit = pred_class_probit_val, Actual = flights_val$DELAYED)

# Logit
roc_logit_val <- roc(flights_val$DELAYED, pred_prob_logit_val)
auc_logit_val <- auc(roc_logit_val)

# Probit
roc_probit_val <- roc(flights_val$DELAYED, pred_prob_probit_val)
auc_probit_val <- auc(roc_probit_val)

# Plot both
plot(roc_logit_val, col = "blue", main = "Out-of-Sample ROC: Logit vs Probit")
lines(roc_probit_val, col = "red")
legend("bottomright", legend = c("Logit", "Probit"), col = c("blue", "red"), lwd = 2)



#print conf matrix
library(caret)

flights_val$DELAYED <- as.factor(flights_val$DELAYED)

conf_logit <- confusionMatrix(as.factor(pred_class_logit_val), flights_val$DELAYED, positive = "1")

# Probit confusion matrix
conf_probit <- confusionMatrix(as.factor(pred_class_probit_val), flights_val$DELAYED, positive = "1")

# View results
conf_logit
conf_probit


# Add predictions and true outcomes to a dataframe
prob_df <- data.frame(
  pred_prob = pred_prob_logit_val,
  actual = as.factor(flights_val$DELAYED)
)
print (prob_df)

# Plot
library(ggplot2)

ggplot(prob_df, aes(x = pred_prob, fill = actual)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  labs(title = "Predicted Probabilities (Logit Model)",
       x = "Predicted Probability of Delay",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values = c("dodgerblue", "firebrick"),
                    name = "Actual Delay",
                    labels = c("No Delay", "Delay"))

ggplot(prob_df, aes(x = pred_prob, fill = actual)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  labs(title = "Predicted Probabilities (Logit Model)",
       x = "Predicted Probability of Delay",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values = c("dodgerblue", "firebrick"),
                    name = "Actual Delay",
                    labels = c("No Delay", "Delay")) +
  xlim(0, 1)  # 👈 this is what forces the full axis


##########################
######## TEST SET ######## 
##########################
flights_test$DELAYED <- as.numeric(as.character(flights_test$DELAYED))

# Logit predictions
pred_prob_logit_test <- predict(logit_model, newdata = flights_test, type = "response")
pred_class_logit_test <- ifelse(pred_prob_logit_test > 0.5, 1, 0)

# Probit predictions
pred_prob_probit_test <- predict(probit_model, newdata = flights_test, type = "response")
pred_class_probit_test <- ifelse(pred_prob_probit_test > 0.5, 1, 0)


# Logit
roc_logit_test <- roc(flights_test$DELAYED, pred_prob_logit_test)
auc_logit_test <- auc(roc_logit_test)

# Probit
roc_probit_test <- roc(flights_test$DELAYED, pred_prob_probit_test)
auc_probit_test <- auc(roc_probit_test)

# Plot both
plot(roc_logit_test, col = "blue", main = "Test ROC Curve: Logit vs Probit")
lines(roc_probit_test, col = "red")
legend("bottomright", legend = c("Logit", "Probit"), col = c("blue", "red"), lwd = 2)

library(caret)

# Make sure test DELAYED is a factor
flights_test$DELAYED <- as.factor(flights_test$DELAYED)

confusionMatrix(as.factor(pred_class_logit_test), flights_test$DELAYED, positive = "1")
confusionMatrix(as.factor(pred_class_probit_test), flights_test$DELAYED, positive = "1")

##########################
######## ACCURACY ######## 
##########################

### LOGIT MODEL ###

# In-sample (balanced)
pred_prob_in_logit <- predict(logit_model, newdata = flights_train_bal, type = "response")
pred_class_in_logit <- ifelse(pred_prob_in_logit > 0.5, 1, 0)
in_error_logit <- mean(pred_class_in_logit != flights_train_bal$DELAYED)

# Out-of-sample (unbalanced)
pred_prob_out_logit <- predict(logit_model, newdata = flights_val, type = "response")
pred_class_out_logit <- ifelse(pred_prob_out_logit > 0.5, 1, 0)
out_sample_error_logit <- mean(pred_class_out_logit != flights_val$DELAYED)


### PROBIT MODEL ###

# In-sample (balanced)
pred_prob_in_probit <- predict(probit_model, newdata = flights_train_bal, type = "response")
pred_class_in_probit <- ifelse(pred_prob_in_probit > 0.5, 1, 0)
in_error_probit <- mean(pred_class_in_probit != flights_train_bal$DELAYED)

# Out-of-sample (unbalanced)
pred_prob_out_probit <- predict(probit_model, newdata = flights_val, type = "response")
pred_class_out_probit <- ifelse(pred_prob_out_probit > 0.5, 1, 0)
out_sample_error_probit <- mean(pred_class_out_probit != flights_val$DELAYED)


### PRINT ERRORS ###
cat("In-sample error (Logit, balanced train):", round(in_error_logit, 4), "\n")
cat("Out-of-sample error (Logit, unbalanced val):", round(out_sample_error_logit, 4), "\n\n")
cat("In-sample error (Probit, balanced train):", round(in_error_probit, 4), "\n")
cat("Out-of-sample error (Probit, unbalanced val):", round(out_sample_error_probit, 4), "\n")




#logit model with balanced data
pred_prob_in_logit <- predict(logit_model, newdata = flights_train_bal, type = "response")
pred_class_in_logit <- ifelse(pred_prob_in_logit > 0.5, 1, 0)
in_error_logit <- mean(pred_class_in_logit != flights_train_bal$DELAYED)
cat("In-sample error (Logit, balanced train):", round(in_error_logit, 4), "\n")

#logit model with UN balanced data
pred_prob_unbal <- predict(model1, newdata = flights_train, type = "response")
pred_class_unbal <- ifelse(pred_prob_unbal > 0.5, 1, 0)
error_unbal <- mean(pred_class_unbal != flights_train$DELAYED)
cat("In-sample error (Logit, unbalanced train):", round(error_unbal, 4), "\n")
 #gives us high accuracy but poor performance on important metrics




confint(logit_model)  #using profiled log-likelihood
confint.default(logit_model) #using standard errors

## confidence interval 
point_conf_table<-cbind(logit_model$coefficients, confint(logit_model))
point_conf_table

##converting w/ base e
exp(point_conf_table)

