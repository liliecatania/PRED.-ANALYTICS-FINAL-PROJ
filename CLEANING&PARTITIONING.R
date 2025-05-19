library(readr)
library(tidyverse)

######################
# IMPORT DATA SETS
######################

# THIS DATA FRAME TELLS US WHICH AIRLINES THE IATA CODES REPRESENT
airlines <- read_csv("~/Desktop/airlines.csv")
dim(airlines)

# THIS DATASET TELLS US WHICH AIRPORTS THE AIRPORT CODES REPRESENT
### TELLS US CITY, AIRPORT, STATE, LATITIUDE, LONGITUDE
airports <- read_csv("~/Desktop/airports.csv")
dim(airports)

# THIS DATA SET CONTAINS ALL FLIGHT INFO
flights <- read_csv("~/Desktop/flights.csv")
dim(flights)

################################
#### EXPLORATORY ANALYSIS ####
###############################
library(ggplot2)
library(tidyr)
library(dplyr)

# VIEW DATA'S STRUCTURE
str(flights)
z
# COUNT MISSING NA'S IN ENTIRE DATA SET
flights_no_na<- flights

dim(flights) #Starting dimensions


sum(is.na(flights)) #starting number of NAs


#############################
# CLEANING :: DEALING WITH NAS
#############################

#REMOVE CANCELLATION_REASON COLUMN (ALL NA)
flights_no_na<- flights_no_na %>%
  select(-CANCELLATION_REASON)

# VIEW NEW DF DIMENSIONS & NUMBER OF RECORDS AND FIELDS LOST
dim(flights_no_na) #dimensions after removing cancellation column

# COUNT NUMBER OF NAs REMAINING
sum(is.na(flights_no_na))

# VIEW COLUMNS WITH REMAINING NA VALUES
na_columns <- colSums(is.na(flights_no_na))
na_columns[na_columns > 0]

# REMOVE ALL ROWS WITH NA IN DEPARTURE DELAY (VARIABLE OF INTEREST)
flights_no_na <- flights_no_na %>%
  filter(!is.na(DEPARTURE_DELAY))

# VIEW NUMBER OF RECORDS AND FILEDS LOST
dim(flights_no_na)

# COUNT NUMBER OF NAS REMOVED
sum(is.na(flights_no_na))


# VIEW COLUMNS WITH REMAINING NA VALUES
na_columns <- colSums(is.na(flights3))
na_columns[na_columns > 0]

# REMOVE ALL ROWS WITH NA IN AIR TIME (BECAUSE FLIGHT DOES NOT EXIST) 
flights_no_na <- flights_no_na %>%
  filter(!is.na(AIR_TIME))

# VIEW NUMBER OF RECORDS AND FILEDS LOST
dim(flights_no_na)

# COUNT NUMBER OF NAS REMOVED
sum(is.na(flights_no_na))

# VIEW COLUMNS WITH REMAINING NA VALUES
na_columns <- colSums(is.na(flights_no_na))
na_columns[na_columns > 0]

# INMPUTE NUMBER 0 FOR NAS IN DELAY TYPE FIELDS
library(tidyr)

flights_no_na <- flights_no_na %>%
  replace_na(list(
    AIR_SYSTEM_DELAY = 0,
    SECURITY_DELAY = 0,                
    AIRLINE_DELAY = 0,
    LATE_AIRCRAFT_DELAY = 0,
    WEATHER_DELAY = 0
  ))

# CONFIRM NO MISSING VALUES REMAIN
sum(is.na(flights_no_na))

# ENSURE SUFFICIENT AMOUNT OF DATA REMAINS
dim(flights_no_na)


#############################
# REMOVE UNECESSARY COLLUMNS
#############################
flights_no_na <- flights_no_na %>%
  select(-ELAPSED_TIME)

flights_no_na <- flights_no_na %>%
  select(-AIR_TIME)

flights_no_na <- flights_no_na %>%
  select(-DIVERTED)

#############################
# REMOVE UNECESSARY ROWS
#############################
flights_no_na <- flights_no_na %>%
  filter(DEPARTURE_DELAY >= 0)


##################
# SPLIT THE DATA
##################

##PARTITIONING THE DATA##
#fraction of sample to be used for training
p<-.7

#number of observations (rows) in the dataframe
obs_count<-dim(flights_no_na)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(123)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

flights_train <- flights_no_na[train_ind, ] #pulls random rows for training
holdout <- flights_no_na[-train_ind, ] #pulls random rows for testing

dim(flights_train)
dim(holdout)

p<-.5

#number of observations (rows) in the dataframe
obs_count<-dim(holdout)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
test_size <- floor(p * obs_count)
test_size
#set the seed to make your partition reproducible
set.seed(123)
#create a vector with the shuffled row numbers of the original dataset
test_ind <- sample(obs_count, size = test_size)

flights_test <- holdout[test_ind, ] #pulls random rows for training
flights_valid <- holdout[-test_ind, ] #pulls random rows for testing

dim(flights_test)
dim(flights_valid)

# CHECK THAT TRAIN IS 70%
(nrow(flights_train))/ nrow(flights_no_na)

#CHECK THAT TEST IS 15%
(nrow(flights_test))/ nrow(flights_no_na)
nrow(flights_test)
#CHECK THAT VALIDATION IS 15%
(nrow(flights_valid))/ nrow(flights_no_na)
nrow(flights_valid)
#CHECK ADDS UP TO ALL ROWS TO MAKE SURE NO OVERLAPPING RECORDS
(nrow(flights_train))+ (nrow(flights_test))+ (nrow(flights_valid)) == nrow(flights_no_na)


library(readr)
# SAVE TO CSV
write_csv(flights_train, "~/Desktop/flights_train.csv")
write_csv(flights_test, "~/Desktop/flights_test.csv")
write_csv(flights_valid, "~/Desktop/flights_val.csv")

nrow(train_data)

str(train_data)
str(valid_data)
str(test_data)






