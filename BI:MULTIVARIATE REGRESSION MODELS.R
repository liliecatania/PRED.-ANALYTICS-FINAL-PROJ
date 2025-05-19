
#########################################
####.  LOAD DATA PARTITIONS       ###
######################################
library(readr)
library(tidyverse)
#LOAD IN MAC
##############################################################################
flights_train<- read_csv("~/Desktop/flights_train.csv")
flights_test<- read_csv("~/Desktop/flights_test.csv")
flights_valid<- read_csv("~/Desktop/flights_val.csv")

# LOAD IN WINDOWS
##############################################################################
flights_train<- read_csv("/Users/svc-olin130lounge/Downloads/flights_train.csv")
flights_test<- read_csv("/Users/svc-olin130lounge/Downloads/flights_test.csv")
flights_valid<- read_csv("/Users/svc-olin130lounge/Downloads/flights_val.csv")


# INSPECT SIZE OF PARTITIONS
dim(flights_train)
dim(flights_test)
dim(flights_valid)

########################################################
#### CORRELATION MATRIX OF NUMERIC VARIABLES ####
########################################################

# SELECT NUMERIC COLUMNS
numeric_data <- flights_train %>%
  dplyr::select(where(is.numeric))

# REMOVE NUMERIC VALUES THAT ARE ACTUALLY CATEGORICAL
numeric_data <- numeric_data %>%
  dplyr::select(-(YEAR:FLIGHT_NUMBER), -CANCELLED)

# COMPUTE CORRELATION MATRIX
cor_matrix <- cor(numeric_data)

# VIEW CORRELATION MATRIX
print(cor_matrix)

# CORRELATION HEAT MAP FOR EASIER ANALYSIS OF CORREL MTRX
#########################################################
library(ggplot2)
library(reshape2)
correlation_melt <- melt(cor_matrix)
colnames(correlation_melt) <- c("Variable1", "Variable2", "Correlation")

num_correl_hm<- ggplot(correlation_melt, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), 
            color = "black", size = 3) +  # Add Correlation Coefficients
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_classic() +
  labs(title = "Correlation Heatmap of Numeric Predictors",
       x = "Predictors", y = "Predictors") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
num_correl_hm

# CORRELATION MTRX, ONLY LOOKING AT CORRS WITH VAR OF INTEREST
#############################################################

# COMPUTE CORRELATION OF ALL INPUT VARIABLES WITH DEPARTURE_DELAY
target_cor <- sapply(numeric_data, function(x) cor(x, flights_train$DEPARTURE_DELAY, use = "complete.obs"))

# CONVERT TO TIDY DATA FRAME
cor_df <- data.frame(Variable = names(target_cor), Cor_Departure_Delay = target_cor)

# SORT BY ABSOLUTE CORRELATION TO GET STRONGEST RELS. FIRST
cor_df <- cor_df %>% arrange(desc(abs(Cor_Departure_Delay)))

# VIEW CORRELATION MATRIX
print(cor_df)

##################################################
####      BIVARIATE REGRESSION MODELING ####
##################################################

# SIMPLE LINEAR BIVARIATE MODEL
##################################################

# BUILD BIVARIATE MODEL PREDICTING DEPARTURE_DELAY WITH AIRLINE DELAY
M1 <- lm(DEPARTURE_DELAY ~ AIRLINE_DELAY, flights_train)

# VIEW MODEL SUMMARY
summary(M1)

# MAKE IN SAMPLE AND OUT OF SAMPLE PREDICITONS
PRED_1_IN <- predict(M1, flights_train)
PRED_1_OUT <- predict(M1, flights_valid)

# COMPUTING  IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
(RMSE_1_IN<-sqrt(sum((PRED_1_IN- flights_train$DEPARTURE_DELAY)^2)/length(PRED_1_IN)))  #computes in-sample error
(RMSE_1_OUT<-sqrt(sum((PRED_1_OUT- flights_valid$DEPARTURE_DELAY)^2)/length(PRED_1_OUT))) #computes out-of-sample 


# BIVARIATE MODEL WITH NON-LINEAR TRANSFORMATION
##############################################################

# ADD QUADRATIC, CUBIC, and QUARTIC POLYNOMIAL TRANSFORMATIONS TO....:

# ... TRAIN SET
flights_train$AIRLINE_DELAY2 <- flights_train$AIRLINE_DELAY^2
flights_train$AIRLINE_DELAY3 <- flights_train$AIRLINE_DELAY^3
flights_train$AIRLINE_DELAY4 <- flights_train$AIRLINE_DELAY^4

# .... VALIDATION SET
flights_valid$AIRLINE_DELAY2 <- flights_valid$AIRLINE_DELAY^2
flights_valid$AIRLINE_DELAY3 <- flights_valid$AIRLINE_DELAY^3
flights_valid$AIRLINE_DELAY4 <- flights_valid$AIRLINE_DELAY^4

# .... TEST SET
flights_test$AIRLINE_DELAY2 <- flights_test$AIRLINE_DELAY^2
flights_test$AIRLINE_DELAY3 <- flights_test$AIRLINE_DELAY^3
flights_test$AIRLINE_DELAY4 <- flights_test$AIRLINE_DELAY^4


# INCORPORATE POLYNOMIAL-TRANSFORMED TERMS INTO BIVARIATE MODEL
M2.1 <- lm(DEPARTURE_DELAY ~ AIRLINE_DELAY +
             AIRLINE_DELAY2 +
             AIRLINE_DELAY3 +
             AIRLINE_DELAY4,
           flights_train)

# VIEW MODEL SUMMARY
summary(M2.1)

#MAKE IN SAMPLE AND OUT OF SAMPLE PREDICTIONS
PRED_2.1_IN <- predict(M2.1, flights_train)
PRED_2.1_OUT <- predict(M2.1, flights_valid)

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
(RMSE_2.1_IN<-sqrt(sum((PRED_2.1_IN- flights_train$DEPARTURE_DELAY)^2)/length(PRED_2.1_IN)))  
(RMSE_2.1_OUT<-sqrt(sum((PRED_2.1_OUT- flights_valid$DEPARTURE_DELAY)^2)/length(PRED_2.1_OUT)))  

# VALIDATION TABLE COMPARING EIN AND EOUT FOR LINEAR AND NON LINEAR BIVARIATE MODEL
##################################################################################################
TABLE_VAL_2b <- as.table(matrix(c(RMSE_1_IN, RMSE_2.1_IN, RMSE_1_OUT, RMSE_2.1_OUT), ncol=2, byrow=TRUE))
colnames(TABLE_VAL_2b) <- c('LINEAR', 'QUARTIC')
rownames(TABLE_VAL_2b) <- c('RMSE_IN', 'RMSE_OUT')
TABLE_VAL_2b 


###################################################
#### IMPLEMENTING REGULARIZATION ON NON LIN BM ###
###################################################

# NOTE: UNREGULARIZED MODEL IS MODEL M2.1

#LOAD PACKAGES
#################################################

library(broom) #FOR tidy() AND glance()
library(MASS) #FOR lm.ridge()
library(lmridge) #FOR lmridge()


# BUILD LM.RIDGE MODELS WHILE REDUCING LAMBDAS BY 66% EACH TIME
###############################################################

########### LAMBDA = 0.75 ########### 
reg1<-lmridge(DEPARTURE_DELAY~AIRLINE_DELAY + AIRLINE_DELAY2 + AIRLINE_DELAY3 +AIRLINE_DELAY4, 
              flights_train, K=0.75)
coef(reg1)

#GENERATE PREDICTIONS WHEN LAMBDA = 0.75
#################################################
PRED_4.1_IN<-predict.lmridge(reg1, flights_train)
PRED_4.1_OUT <- predict.lmridge(reg1, flights_valid)

# BENCHMARK IN AND OUT WHEN LAMBDA = 0.75
#################################################
(RMSE_4.1_IN<-sqrt(sum((PRED_4.1_IN-flights_train$DEPARTURE_DELAY)^2)/length(PRED_4.1_IN)))  #computes in-sample error
(RMSE_4.1_OUT<-sqrt(sum((PRED_4.1_OUT-flights_valid$DEPARTURE_DELAY)^2)/length(PRED_4.1_OUT)))


########### LAMBDA = 0.25 ########### 
reg2<-lmridge(DEPARTURE_DELAY~AIRLINE_DELAY + AIRLINE_DELAY2 + AIRLINE_DELAY3 +AIRLINE_DELAY4,
              flights_train, K=.25)
coef(reg2)

# GENERATE PREDICTIONS WHEN LAMBDA = 0.25
#################################################
PRED_4.2_IN<-predict.lmridge(reg2, flights_train)
PRED_4.2_OUT <- predict.lmridge(reg2, flights_valid)

# BENCHMARK IN AND OUT WHEN LAMBDA = 0.25
#################################################
(RMSE_4.2_IN<-sqrt(sum((PRED_4.2_IN-flights_train$DEPARTURE_DELAY)^2)/length(PRED_4.2_IN)))  #computes in-sample error
(RMSE_4.2_OUT<-sqrt(sum((PRED_4.2_OUT-flights_valid$DEPARTURE_DELAY)^2)/length(PRED_4.2_OUT)))


########### LAMBDA = 0.075 ########### 
reg3<-lmridge(DEPARTURE_DELAY~AIRLINE_DELAY + AIRLINE_DELAY2 + AIRLINE_DELAY3 +AIRLINE_DELAY4,
              flights_train, K=.075)
coef(reg3)

# GENERATE PREDICTIONS WHEN LAMBDA = 0.075
#################################################
PRED_4.3_IN<-predict.lmridge(reg3, flights_train)
PRED_4.3_OUT <- predict.lmridge(reg3, flights_valid)

# BENCHMARK IN AND OUT WHEN LAMBDA = 0.075
#################################################
(RMSE_4.3_IN<-sqrt(sum((PRED_4.3_IN-flights_train$DEPARTURE_DELAY)^2)/length(PRED_4.3_IN)))  #computes in-sample error
(RMSE_4.3_OUT<-sqrt(sum((PRED_4.3_OUT-flights_valid$DEPARTURE_DELAY)^2)/length(PRED_4.3_OUT)))


########### LAMBDA = 0.025 ########### 
reg4<-lmridge(DEPARTURE_DELAY~AIRLINE_DELAY + AIRLINE_DELAY2 + AIRLINE_DELAY3 +AIRLINE_DELAY4, flights_train, K=.025)

coef(reg4)

# GENERATE PREDICTIONS WHEN LAMBDA = 0.025
#################################################
PRED_4.4_IN<-predict.lmridge(reg4, flights_train)
PRED_4.4_OUT <- predict.lmridge(reg4, flights_valid)

# BENCHMARK IN AND OUT WHEN LAMBDA = 0.025
#################################################
(RMSE_4.4_IN<-sqrt(sum((PRED_4.4_IN-flights_train$DEPARTURE_DELAY)^2)/length(PRED_4.4_IN)))  #computes in-sample error
(RMSE_4.4_OUT<-sqrt(sum((PRED_4.4_OUT-flights_valid$DEPARTURE_DELAY)^2)/length(PRED_4.4_OUT)))

# BENCHMARK RIDGE MODELS ACROSS ALL 4 LAMBDA VALUES AND UNREGULARIZED MODEL
##################################################################################################
TABLE_VAL_RIDGE<- as.table(matrix(c(RMSE_4.1_IN, RMSE_4.2_IN, RMSE_4.3_IN, RMSE_4.4_IN, RMSE_2.1_IN, RMSE_4.1_OUT, RMSE_4.2_OUT, RMSE_4.3_OUT, RMSE_4.4_OUT, RMSE_2.1_OUT), ncol=5, byrow=TRUE))
colnames(TABLE_VAL_RIDGE) <- c("K=0.75", 'K=0.25', 'K=0.075', 'K=0.025', 'UNREG')
rownames(TABLE_VAL_RIDGE) <- c('RMSE_IN', 'RMSE_OUT')
TABLE_VAL_RIDGE


# PLOT IN AND ESTIMATED OUT OF SAMPLE ERROR AS REGULARIZATION INCREASES
##################################################################################################

# TABLE_VAL_RIDGE AS DATA FRAME
#################################################
TABLE_VAL_RIDGE <- as.table(matrix(c(40.25567, 38.64745, 38.18686, 38.00906, 
                                     40.42987, 38.81153, 38.34554, 38.16800), 
                                   ncol=4, byrow=TRUE))

colnames(TABLE_VAL_RIDGE) <- c("K=0.75", "K=0.25", "K=0.075", "K=0.025")
rownames(TABLE_VAL_RIDGE) <- c("RMSE_IN", "RMSE_OUT")
df_ridge <- as.data.frame(as.table(TABLE_VAL_RIDGE))
colnames(df_ridge) <- c("RMSE_Type", "K_Value", "RMSE")

# CONVERT K VALUES FOR LAMBDAS INTO NUMERIC VALUES
#################################################
df_ridge$K_Value <- as.numeric(gsub("K=", "", df_ridge$K_Value))

# PLOT
#################################################
ridge_rmse_plot<- ggplot(df_ridge, aes(x = K_Value, y = RMSE, color = RMSE_Type, group = RMSE_Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_reverse() + 
  geom_hline(yintercept = c(38.00906,38.16800), linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = max(df_ridge$K_Value), y = 38.16800, 
           label = "Unregularized model's RMSE (Eout) = 38.16800", color = "black", hjust = 0.1, vjust = -1)+
  annotate("text", x = max(df_ridge$K_Value), y = 38.00906, 
           label = "Unregularized model's RMSE (Ein) = 38.00906", color = "black", hjust = 0.1 , vjust = -1)+
  theme_minimal() +
  labs(
    title = "RIDGE Model's In-Sample and Out-of-Sample RMSE vs. 4 Values of Lambda",
    x = "K (Regularization Strength)",
    y = "RMSE",
    color = "RMSE Type" ) +
  theme(legend.position = "top") +
  scale_y_continuous(limits = c(38, 41))  # Manually set Y-axis range
ridge_rmse_plot

#################################################
#### TASK 2.4c : GAM MODEL - SPLINE ####
#################################################

#LOAD LIBRARY
#################################################
library(mgcv)

# BUILD SPLINE MODEL
#################################################
M5 <- gam(DEPARTURE_DELAY ~ s(AIRLINE_DELAY), data = flights_train, family = 'gaussian')
summary(M5) #generates summary diagnostic output

# MAKE IN SAMPLE AND OUT OF SAMPLE PREDICITIONS
#################################################
PRED_5_IN <- predict(M5, flights_train, type = 'response')
PRED_5_OUT <- predict(M5, flights_valid, type = 'response')

# CALCULATE IN SAMPLE AND OUT OF SAMPLE ERROR
#################################################
(RMSE_5_IN<-sqrt(sum((PRED_5_IN-flights_train$DEPARTURE_DELAY)^2)/length(PRED_5_IN)))  #computes in-sample error
(RMSE_5_OUT<-sqrt(sum((PRED_5_OUT-flights_valid$DEPARTURE_DELAY)^2)/length(PRED_5_OUT)))

# BENCH MARK ACROSS ALL PREVIOUS MODELS
#################################################
TABLE_VAL_SPLINE<- as.table(matrix(c(RMSE_1_IN, RMSE_2.1_IN, RMSE_4.4_IN, RMSE_5_IN, RMSE_1_IN, RMSE_2.1_OUT, RMSE_4.4_OUT, RMSE_5_OUT), ncol=4, byrow=TRUE))
colnames(TABLE_VAL_SPLINE) <- c("LINEAR", 'QUARTIC', 'RIDGE', 'SPLINE')
rownames(TABLE_VAL_SPLINE) <- c('RMSE_IN', 'RMSE_OUT')
TABLE_VAL_SPLINE

########################################################
###PLOTTING THE REGRESSION MODELS AGAINST ONE ANOTHER###
########################################################

# CREATE GRID OF X AXIS VALUES
x_grid<- seq(0,2000,.1)

# PLOT TRAINING DATA PARTITION
plot(flights_train$DEPARTURE_DELAY~flights_train$AIRLINE_DELAY, col="blue")

# MODEL FITTED VALUES
predictions_1<- predict(M1, list(AIRLINE_DELAY=x_grid))
predictions_2<- predict(M2.1, list(AIRLINE_DELAY=x_grid, AIRLINE_DELAY2=x_grid^2, AIRLINE_DELAY3=x_grid^3, AIRLINE_DELAY4=x_grid^4))
predictions_3<- predict(reg4, list(AIRLINE_DELAY=x_grid, AIRLINE_DELAY2=x_grid^2, AIRLINE_DELAY3=x_grid^3, AIRLINE_DELAY4=x_grid^4), type='response')
predictions_4<- predict(M5, list(AIRLINE_DELAY = x_grid), type='response')

# PLOT MODELS
lines(x_grid, predictions_1, col='darkgreen', lwd=3) # LINEAR
lines(x_grid, predictions_2, col='green', lwd=3) # 4TH ORDER POLY
lines(x_grid, predictions_3, col='purple', lwd=3) # RIDGE
lines(x_grid, predictions_4, col='orange', lwd=3) # SPLINE

#PLOT VALIDATION PARTITION
points(flights_valid$DEPARTURE_DELAY~flights_valid$AIRLINE_DELAY, col = 'red', pch=3, cex=.5)

####################################################################################################
# USE UNCONTAMINATED PORTION TO REPORT ESTIMATED OUT OF SAMPLE ERROR ON BEST VAL MODEL
####################################################################################################

# MODEL WITH BEST OUT OF SAMPLE PERFORMANCE ON VALIDATION PARITION: LINEAR MODE
# ESTIMATE OUT OF SAMPLE ERROR ON LINEAR MODEL

PRED_M1_TEST<- predict(M1, flights_test)
(RMSE_M1_TEST<-sqrt(sum((PRED_M1_TEST-flights_test$DEPARTURE_DELAY)^2)/length(PRED_M1_TEST)))


# BENCH MARK LINEAR MODEL PERFROMANCE ACROSS ALL PARTITIONS
TABLE_CHOSEN<- as.table(matrix(c(RMSE_1_IN, RMSE_1_OUT, RMSE_M1_TEST), ncol=3, byrow=TRUE))
colnames(TABLE_CHOSEN) <- c("EIN", 'E[EOUT]', 'EOUT')
rownames(TABLE_CHOSEN) <- c('RMSE')
TABLE_CHOSEN

############################# END BIVARIATE MODELING #######################################


##################################################
####  MULTIVARIATE REGRESSION MODELS ####
##################################################

# BUILD MULTIVARIATE LINEAR MODEL WITHOUT NON-LINEAR TRANSFORMATIONS
##############################################################################
MM1 <- lm(DEPARTURE_DELAY ~ AIRLINE_DELAY + LATE_AIRCRAFT_DELAY + WEATHER_DELAY +AIR_SYSTEM_DELAY, flights_train)

# VIEW MODEL SUMMARY
summary(MM1)

# MAKE IN SAMPLE AND OUT OF SAMPLE PREDICITIONS
#################################################
PRED_MM1_IN <- predict(MM1, flights_train)
PRED_MM1_OUT <- predict(MM1, flights_valid)

# CALCULATE IN SAMPLE AND OUT OF SAMPLE ERROR
#################################################
(RMSE_MM1_IN<-sqrt(sum((PRED_MM1_IN- flights_train$DEPARTURE_DELAY)^2)/length(PRED_MM1_IN)))  #computes in-sample error
(RMSE_MM1_OUT<-sqrt(sum((PRED_MM1_OUT- flights_valid$DEPARTURE_DELAY)^2)/length(PRED_MM1_OUT))) #computes out-of-sample 

# CHECK FOR MULTICOLINEARITY
vif_values <- car::vif(MM1)
print(vif_values)

#  REGULARIZE MULTIVARIATE REGRESSION MODEL
##############################################################################

# NOTE: UNREGULARIZED MODEL IS MODEL MM1

# BUILD LM.RIDGE MODELS WHILE REDUCING LAMBDAS BY 66% EACH TIME
###############################################################

########### LAMBDA = 0.75 ########### 
MMR1<-lmridge(DEPARTURE_DELAY~AIRLINE_DELAY +  LATE_AIRCRAFT_DELAY +
                WEATHER_DELAY + AIR_SYSTEM_DELAY, flights_train, K=.75)

coef(MMR1)
PRED_R1_IN<-predict(MMR1, flights_train)
PRED_R1_OUT <- predict(MMR1, flights_valid)

(RMSE_R1_IN<-sqrt(sum((PRED_R1_IN-flights_train$DEPARTURE_DELAY)^2)/length(PRED_R1_IN)))  #computes in-sample error
(RMSE_R1_OUT<-sqrt(sum((PRED_R1_OUT-flights_valid$DEPARTURE_DELAY)^2)/length(PRED_R1_OUT)))

########### LAMBDA = 0.25 ########### 
MMR2<-lmridge(DEPARTURE_DELAY~AIRLINE_DELAY +  LATE_AIRCRAFT_DELAY +
                WEATHER_DELAY + AIR_SYSTEM_DELAY, flights_train, K=.25)

coef(MMR2)
PRED_R2_IN<-predict(MMR2, flights_train)
PRED_R2_OUT <- predict(MMR2, flights_valid)

(RMSE_R2_IN<-sqrt(sum((PRED_R2_IN-flights_train$DEPARTURE_DELAY)^2)/length(PRED_R2_IN)))  #computes in-sample error
(RMSE_R2_OUT<-sqrt(sum((PRED_R2_OUT-flights_valid$DEPARTURE_DELAY)^2)/length(PRED_R2_OUT)))

########### LAMBDA = 0.075 ########### 
MMR3<-lmridge(DEPARTURE_DELAY~AIRLINE_DELAY +  LATE_AIRCRAFT_DELAY +
                WEATHER_DELAY + AIR_SYSTEM_DELAY, flights_train, K=.075)

coef(MMR3)
PRED_R3_IN<-predict(MMR3, flights_train)
PRED_R3_OUT <- predict(MMR3, flights_valid)

(RMSE_R3_IN<-sqrt(sum((PRED_R3_IN-flights_train$DEPARTURE_DELAY)^2)/length(PRED_R3_IN)))  #computes in-sample error
(RMSE_R3_OUT<-sqrt(sum((PRED_R3_OUT-flights_valid$DEPARTURE_DELAY)^2)/length(PRED_R3_OUT)))

########### LAMBDA = 0.025 ########### 
MMR4<-lmridge(DEPARTURE_DELAY~AIRLINE_DELAY +  LATE_AIRCRAFT_DELAY +
                WEATHER_DELAY + AIR_SYSTEM_DELAY, flights_train, K=.025)

coef(MMR4)
PRED_R4_IN<-predict(MMR4, flights_train)
PRED_R4_OUT <- predict(MMR4, flights_valid)

(RMSE_R4_IN<-sqrt(sum((PRED_R4_IN-flights_train$DEPARTURE_DELAY)^2)/length(PRED_R4_IN)))  #computes in-sample error
(RMSE_R4_OUT<-sqrt(sum((PRED_R4_OUT-flights_valid$DEPARTURE_DELAY)^2)/length(PRED_R4_OUT)))


# BENCHMARK RIDGE MODELS ACROSS ALL 4 LAMBDA VALUES AND UNREGULARIZED MODEL
##################################################################################################
TABLE_VAL_MM_RIDGE<- as.table(matrix(c(RMSE_R1_IN, RMSE_R2_IN, RMSE_R3_IN, RMSE_R4_IN, RMSE_MM1_IN,
                                    RMSE_R1_OUT, RMSE_R2_OUT, RMSE_R3_OUT, RMSE_R4_OUT, RMSE_MM1_OUT), ncol=5, byrow=TRUE))
colnames(TABLE_VAL_MM_RIDGE) <- c("K=0.75", 'K=0.25', 'K=0.075', 'K=0.025', 'UNREG')
rownames(TABLE_VAL_MM_RIDGE) <- c('RMSE_IN', 'RMSE_OUT')
TABLE_VAL_MM_RIDGE

# RECREATE VAL TABLE AS A DATA FRAME
TABLE_VAL_MM_RIDGE <- as.table(matrix(c(21.73083, 15.06338, 12.07047,11.65685, 
                                     23.74574, 15.00398, 11.98404, 11.57112), 
                                   ncol=4, byrow=TRUE))
colnames(TABLE_VAL_MM_RIDGE) <- c("K=0.75", "K=0.25", "K=0.075", "K=0.025")
rownames(TABLE_VAL_MM_RIDGE) <- c("RMSE_IN", "RMSE_OUT")

# CONVERT TABLE TO DATAFRAME
df_ridge2 <- as.data.frame(as.table(TABLE_VAL_MM_RIDGE))
colnames(df_ridge2) <- c("RMSE_Type", "K_Value", "RMSE")

# CONVERT K VALUES TO NUMERIC
df_ridge2$K_Value <- as.numeric(gsub("K=", "", df_ridge2$K_Value))

# PLOT
ridge_rmse_plot2<- ggplot(df_ridge2, aes(x = K_Value, y = RMSE, color = RMSE_Type, group = RMSE_Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_reverse() + 
  geom_hline(yintercept = c(11.51586, 11.59823), linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = max(df_ridge$K_Value), y = 11.51586, 
           label = "Unregularized model's RMSE (Eout) = 11.51586", color = "black", hjust = 0.025, vjust = -0.5)+
  theme_minimal() +
  labs(
    title = "MRIDGE Model's In-Sample and Out-of-Sample RMSE vs. 4 Values of Lambda",
    x = "Regularization Strength (K)",
    y = "RMSE",
    color = "RMSE Type" ) +
  theme(legend.position = "top") +
  scale_y_continuous(limits = c(11, 24))  # Manually set Y-axis range
ridge_rmse_plot2

 


# NON-LINEAR MULTIVARIATE REGRESSION MODEL WITH NO REGULARIZATION
################################################################################

# VIEW SCATTER PLOT OF EACH VARS RELATIONSHIP WITH DEPARTURE_DELAYS
################################################################################

# PLOT AIRLINE_DELAY VS DEPARTURE_DELAY
airline_delay_plot <- ggplot(flights_train, aes(AIRLINE_DELAY, DEPARTURE_DELAY)) +
                               geom_point()
airline_delay_plot

# PLOT LATE_AIRCRAFT_DELAY VS DEPARTURE_DELAY
late_aircraft_delay_plot <- ggplot(flights_train, aes(LATE_AIRCRAFT_DELAY, DEPARTURE_DELAY)) +
  geom_point()
late_aircraft_delay_plot

# PLOT WEATHER_DELAY VS DEPARTURE_DELAY
weather_delay_plot <- ggplot(flights_train, aes(WEATHER_DELAY, DEPARTURE_DELAY)) +
  geom_point()
weather_delay_plot

# PLOT AIR_SYSTEM_DELAY VS DEPARTURE_DELAY
airsystem_delay_plot <- ggplot(flights_train, aes(AIR_SYSTEM_DELAY, DEPARTURE_DELAY)) +
  geom_point()
airsystem_delay_plot


# ADD COLUMN OF QUADRATIC TRANSFORMATION TO ALL SETS & VARS
##############################################################

#       AIRLINE DELAY
#QUADRATIC TRANSFORMATION OF AIRLINE DELAY (train)
flights_train$AIRLINE_DELAY2 <- flights_train$AIRLINE_DELAY^2 
#QUADRATIC TRANSFORMATION OF AIRLINE DELAY (test)
flights_test$AIRLINE_DELAY2 <- flights_test$AIRLINE_DELAY^2 
#QUADRATIC TRANSFORMATION OF AIRLINE DELAY (val)
flights_valid$AIRLINE_DELAY2 <- flights_valid$AIRLINE_DELAY^2

#       LATE AIRCRAFT DELAY
#QUADRATIC TRANSFORMATION OF LATE AIRCRAFT DELAY (train)
flights_train$LATE_AIRCRAFT_DELAY2 <- flights_train$LATE_AIRCRAFT_DELAY^2 
#QUADRATIC TRANSFORMATION OF LATE AIRCRAFT DELAY (test)
flights_test$LATE_AIRCRAFT_DELAY2 <- flights_test$LATE_AIRCRAFT_DELAY^2 
#QUADRATIC TRANSFORMATION OF LATE AIRCRAFT DELAY (val)
flights_valid$LATE_AIRCRAFT_DELAY2 <- flights_valid$LATE_AIRCRAFT_DELAY^2

#       WEATHER DELAY
#QUADRATIC TRANSFORMATION OF WEATHER DELAY (train)
flights_train$WEATHER_DELAY2 <- flights_train$WEATHER_DELAY^2 
#QUADRATIC TRANSFORMATION OF WEATHER DELAY (test)
flights_test$WEATHER_DELAY2 <- flights_test$WEATHER_DELAY^2 
#QUADRATIC TRANSFORMATION OF WEATHER DELAY (val)
flights_valid$WEATHER_DELAY2 <- flights_valid$WEATHER_DELAY^2


#         AIR SYSTEM DELAY
#QUADRATIC TRANSFORMATION OF AIR SYSTEM DELAY (train)
flights_train$AIR_SYSTEM_DELAY2 <- flights_train$AIR_SYSTEM_DELAY^2 
#QUADRATIC TRANSFORMATION OF AIR SYSTEM DELAY (test)
flights_test$AIR_SYSTEM_DELAY2 <- flights_test$AIR_SYSTEM_DELAY^2 
#QUADRATIC TRANSFORMATION OF AIR SYSTEM DELAY (val)
flights_valid$AIR_SYSTEM_DELAY2 <- flights_valid$AIR_SYSTEM_DELAY^2

#   VERIFY THAT ALL COLUMNS WERE ADDED SUCCESSFULLY 
dim(flights_train)
dim(flights_test)
dim(flights_valid)


#  NON-LIN MULTI-VAR MODEL
##############################

# BUILD MODEL
NLMM<-lm(DEPARTURE_DELAY~AIRLINE_DELAY + AIRLINE_DELAY2 + 
           LATE_AIRCRAFT_DELAY +  LATE_AIRCRAFT_DELAY2 +
           WEATHER_DELAY + WEATHER_DELAY2 +
           AIR_SYSTEM_DELAY + AIR_SYSTEM_DELAY2, flights_train)

# VIEW MODEL SUMMARY
summary(NLMM)

# MAKE PREDICTIONS ON TRAINING AND VALIDATION PARTITIONS
PRED_NLMM_IN<-predict(NLMM, flights_train)
PRED_NLMM_OUT <- predict(NLMM, flights_valid)

# COMPUTE IN SAMPLE AND ESTIMATED OUT OF SAMPLE ERROR
(RMSE_NLMM_IN<-sqrt(sum((PRED_NLMM_IN-flights_train$DEPARTURE_DELAY)^2)/length(PRED_NLMM_IN)))  
(RMSE_NLMM_OUT<-sqrt(sum((PRED_NLMM_OUT-flights_valid$DEPARTURE_DELAY)^2)/length(PRED_NLMM_OUT)))

# CHECK FOR MULTICOLINEARITY ON MM54 (BEST RIDGE MODEL)
vif_values <- car::vif(NLMM)
print(vif_values)


# NON LIN MULTI-VAR MODEL: SPLINE
#################################################
M5a <- gam(DEPARTURE_DELAY ~ s(AIRLINE_DELAY) +
             s(LATE_AIRCRAFT_DELAY) +
             s(WEATHER_DELAY)+
             s(AIR_SYSTEM_DELAY), data = flights_train, family = 'gaussian')
summary(M5a) #generates summary diagnostic output

# MAKE IN SAMPLE AND OUT OF SAMPLE PREDICITIONS
#################################################
PRED_5A_IN <- predict(M5a, flights_train, type = 'response')
PRED_5A_OUT <- predict(M5a, flights_valid, type = 'response')

# CALCULATE IN SAMPLE AND OUT OF SAMPLE ERROR
#################################################
(RMSE_5A_IN<-sqrt(sum((PRED_5A_IN-flights_train$DEPARTURE_DELAY)^2)/length(PRED_5A_IN)))  #computes in-sample error
(RMSE_5A_OUT<-sqrt(sum((PRED_5A_OUT-flights_valid$DEPARTURE_DELAY)^2)/length(PRED_5A_OUT)))

# BENCH MARK ACROSS ALL PREVIOUS MODELS
#################################################
TABLE_VAL_MM_NONLIN <- as.table(matrix(c(RMSE_MM1_IN, RMSE_R4_IN, RMSE_5A_IN, 
                                         RMSE_MM1_OUT, RMSE_R4_OUT, RMSE_5A_OUT), ncol=3, byrow=TRUE))
colnames(TABLE_VAL_MM_NONLIN) <- c('LINEAR', 'REGULARIZED','SPLINE' )
rownames(TABLE_VAL_MM_NONLIN) <- c('RMSE_IN', 'RMSE_OUT')
TABLE_VAL_MM_NONLIN
# CREATE DATA FRAME OF PREDICTIONS AND ACTUAL VALUES FOR THE TEST SET
MM_df <- data.frame(
  Actual = flights_valid$DEPARTURE_DELAY,  
  Pred_ModelMM1 = PRED_MM1_OUT,
  Pred_ModelR4 = PRED_R4_OUT,
  Pred_ModelSPLINE= PRED_5A_OUT)

# CREATE PLOT OF ACTUAL VS PREDICTED VALUES FOR EACH MODEL
MM_predvsactual<- ggplot(MM_df, aes(x = Actual)) +
  geom_point(aes(y = Pred_ModelMM1, color = "LINEAR"), alpha = 0.7) +
  geom_point(aes(y = Pred_ModelR4, color = "RIDGE"), alpha = 0.4) +
  geom_point(aes(y = Pred_ModelSPLINE, color = "SPLINE"), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  labs(title = "Predicted vs Actual Values",
       x = "Actual Values (Departure Delay)",
       y = "Predicted Values (Departure Delay") +
  scale_color_manual(values = c("LINEAR" = "black", "RIDGE" = "purple", "SPLINE" = "red")) +
  theme_minimal()

MM_predvsactual


#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### TASK 2.5d : ESTIMATE SUPPORT VECTOR MACHINE REGRESSION ####
#### #### #### #### #### #### #### #### #### #### #### #### #### 

# LOAD LIBRARIES FOR SVM MODELS
####################################################

library(e1071)
library(dplyr)

# IMPORT RE-SHUFFLED PARTITIONS ON MAC
##############################################################################
flights_train_svm <- read_csv("~/Desktop/flights_train_SVM.csv")
flights_test_svm <- read_csv("~/Desktop/flights_test_SVM.csv")
flights_valid_svm <- read_csv("~/Desktop/flights_val_SVM.csv")

# LOAD IN WINDOWS
##############################################################################
flights_train<- read_csv("/Users/svc-olin130lounge/Downloads/flights_train_SVM.csv")
flights_test<- read_csv("/Users/svc-olin130lounge/Downloads/flights_test_SVM.csv")
flights_valid<- read_csv("/Users/svc-olin130lounge/Downloads/flights_val_SVM.csv")

# INVETSIGATE SKEWNESS OF NEW PARTITIONS
summary(flights_train_svm$DEPARTURE_DELAY)
summary(flights_valid_svm$DEPARTURE_DELAY)

# ELIMINATE OUTLIERS TO ENSURE MODEL DOES NOT TRY TO FIT THEM
Q1t <- quantile(flights_train_svm$DEPARTURE_DELAY, .25)
Q1v <- quantile(flights_valid_svm$DEPARTURE_DELAY, .25)

Q3t <- quantile(flights_train_svm$DEPARTURE_DELAY, .75)
Q3v <- quantile(flights_valid_svm$DEPARTURE_DELAY, .75)

IQRt <- IQR(flights_train_svm$DEPARTURE_DELAY)
IQRv <- IQR(flights_valid_svm$DEPARTURE_DELAY)

cleanedt<- subset(flights_train_svm, flights_train_svm$DEPARTURE_DELAY > (Q1t - 1.5*IQRt) & flights_train_svm$DEPARTURE_DELAY < (Q3t + 1.5*IQRt))
dim(cleanedt)
cleanedv<- subset(flights_valid_svm, flights_valid_svm$DEPARTURE_DELAY > (Q1v - 1.5*IQRv) & flights_valid_svm$DEPARTURE_DELAY < (Q3v + 1.5*IQRv))
dim(cleanedv)

# VIEW NEW SUMMARY OF DATASETS AFTER OUTLIER REMOVAL
summary(cleanedt$DEPARTURE_DELAY)
summary(cleanedv$DEPARTURE_DELAY)
# SELECT FEATURES FROM TRAINING SET
####################################################
PCA_train<- cleanedt %>%
  dplyr::select(DEPARTURE_DELAY, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY, WEATHER_DELAY, AIR_SYSTEM_DELAY)
head(PCA_train)

# SELECT FEATURES FROM VALIDATION SET
####################################################
PCA_valid<- cleanedv %>%
  dplyr::select(DEPARTURE_DELAY, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY, WEATHER_DELAY, AIR_SYSTEM_DELAY)
head(PCA_valid)

#STORE Y VAR AND REMOVE FROM PARTITIONS FOR PCA TRAINING AND TESTING
#############################################################
flights_train.y <- PCA_train$DEPARTURE_DELAY

PCA_train$DEPARTURE_DELAY<- NULL

flights_valid.y <- PCA_valid$DEPARTURE_DELAY

PCA_valid$DEPARTURE_DELAY <- NULL


# RUN PCA
####################################################
flights_train.pc1<- prcomp(PCA_train, center=TRUE)
summary(flights_train.pc1)
as.matrix(PCA_train)%*%as.matrix(flights_train.pc1$rotation)[,1:4]%>%head(10)

flights_valid.pc1 <- prcomp(PCA_valid, center=TRUE)
summary(flights_valid.pc1)
as.matrix(PCA_valid)%*%as.matrix(flights_valid.pc1$rotation)[,1:4]%>%head(10)

#ADD BACK Y VAR COLUMN
####################################################
pcs_train<- as.data.frame(flights_train.pc1$x)
ols.data<- cbind(flights_train.y, pcs_train)

pcs_valid<- as.data.frame(flights_valid.pc1$x)
ols.data_valid<- cbind(flights_valid.y,pcs_valid)


# GRAB A SUBSET OF THE DATA FOR COMPUTATIONAL EFFICIENCY
########################################################

ols.data_svm <- ols.data[1:20000, ]
summary(ols.data_svm$flights_train.y)
summary(ols.data_valid$flights_valid.y)
ols.data_valid_svm <- ols.data_valid[1:2000, ]

# SVM, Y REGRESSED ON 2 PRINCIPLE COMPONENTS
####################################################
kern_type<- 'linear'
SVM_MM2<- svm(flights_train.y~ PC1 +PC2, 
              data = ols.data_svm, 
              type = "eps-regression", #set to "eps-regression" for numeric prediction
              kernel = kern_type,
              cost=1,                   #REGULARIZATION PARAMETER
              gamma = 1/(ncol(ols.data_svm)-1), #DEFAULT KERNEL PARAMETER
              coef0 = 0,                    #DEFAULT KERNEL PARAMETER
              degree=2,                     #POLYNOMIAL KERNEL PARAMETER
              scale = FALSE)                #RESCALE DATA? (SET TO TRUE TO NORMALIZE)

summary(SVM_MM2)

# MAKE IN AND OUT OF SAMPLE PREDICTIONS, THEN UN-NORMALIZE THEM
##############################################################################
PRED_SVM_IN<-predict(SVM_MM2, ols.data_svm)
PRED_SVM_OUT <- predict(SVM_MM2, ols.data_valid_svm)


# COMPUTE IN SAMPLE AND ESTIMATED OUT OF SAMPLE ERROR
##############################################################################
(RMSE_SVM_IN<-sqrt(sum((PRED_SVM_IN-ols.data_svm$flights_train.y)^2)/length(PRED_SVM_IN))) 
(RMSE_SVM_OUT<-sqrt(sum((PRED_SVM_OUT-ols.data_valid_svm$flights_valid.y)^2)/length(PRED_SVM_OUT)))

# VIEW RESULTS IN A TABLE
TABLE_VAL_SVM <- as.table(matrix(c(RMSE_SVM_IN, RMSE_SVM_OUT), ncol=2, byrow=TRUE))
colnames(TABLE_VAL_SVM) <- c('RMSE_IN', 'RMSE_OUT')
TABLE_VAL_SVM

pca_scores<- as.data.frame(PCA_train$x)
# Plotting the first two principal components
ggplot(pcs_train, aes(x = PC1, y = PC2)) +
  geom_point(color = "blue", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Principal Component Score Plot (PC1 vs. PC2)",
       x = "Principal Component 1", 
       y = "Principal Component 2")
#### #### #### #### #### #### #### #### #### #### #### #### #### 
####  REGRESSION TREE ####
#### #### #### #### #### #### #### #### #### #### #### #### #### 

library(tidymodels) #INCLUDES parsnip PACKAGE FOR decision_tree()
library(caret) #FOR confusionMatrix()
library(rpart.plot)

#SPECIFYING THE REGRESSION TREE MODEL
reg_spec <- decision_tree(min_n = 1000 , #minimum number of observations for split
                          tree_depth = 30, #max tree depth
                          cost_complexity = 0.01)  %>% #regularization parameter
  set_engine("rpart") %>%
  set_mode("regression")
print(reg_spec)

# REGRESSION TREE FORMULA
reg_fmla <- DEPARTURE_DELAY ~ AIRLINE_DELAY + LATE_AIRCRAFT_DELAY + WEATHER_DELAY + AIR_SYSTEM_DELAY

# BUILD REGRESSION TREE MODEL
reg_tree <- reg_spec %>%
  fit(formula = reg_fmla, data = flights_train)
print(reg_tree)

# PLOT REGRESSION TREE
reg_tree$fit %>%
  rpart.plot(type = 4, roundint = FALSE)

library(yardstick)
pred_regin <- predict(reg_tree, new_data = flights_train) %>%
  bind_cols(flights_train)

# CALCULATE RMSE USING YARDSTICK & BIND PREDICTIONS
rmse_resultin <- yardstick::rmse(pred_regin, DEPARTURE_DELAY, .pred)
print(rmse_resultin)
rmse_resultin<- as.data.frame(rmse_resultin)
(RMSE_RT_IN<- rmse_resultin[ ,3])

pred_regout <- predict(reg_tree, new_data = flights_valid) %>%
  bind_cols(flights_valid)
rmse_resultout <- yardstick::rmse(pred_regout, DEPARTURE_DELAY, .pred)
rmse_resultout<- as.data.frame(rmse_resultout)
print(rmse_resultout)
(RMSE_RT_OUT<- rmse_resultout[ ,3])

# USE SCREE PLOT TO EVALUATE TUNING
plotcp(reg_tree$fit, minline=TRUE)




library(tidymodels)
library(yardstick)
detach("package:caret", unload = TRUE)
tree_spec <- decision_tree(min_n = tune(),
                           tree_depth = tune(),
                           cost_complexity= tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

#  CREATE A REGULAR GRID FOR THE SPECIFIED PARAMATERS
tree_grid <- grid_regular(parameters(tree_spec), levels = 3)

set.seed(123) #SET SEED FOR REPRODUCIBILITY WITH CROSS-VALIDATION
tune_results <- tune_grid(tree_spec,
                          reg_fmla, #MODEL FORMULA
                          resamples = vfold_cv(flights_train, v=3), #RESAMPLES / FOLDS
                          grid = tree_grid, #GRID
                          metrics = metric_set(rmse)) #BENCHMARK METRIC

best_params <- select_best(tune_results)
best_params

final_spec <- finalize_model(tree_spec, best_params)
final_model <- final_spec %>% fit(reg_fmla, flights_train)
final_model$fit 
final_model$ordered

library(yardstick)
# CALCULATE RMSE USING YARDSTICK & BIND PREDICTIONS
pred_regintune <- predict(final_model, new_data = flights_train) %>%
  bind_cols(flights_train)

rmse_resultintune <- yardstick::rmse(pred_regintune, DEPARTURE_DELAY, .pred)
print(rmse_resultintune)
rmse_resultintune<- as.data.frame(rmse_resultintune)
(RMSE_RT_IN2<- rmse_resultintune[ ,3])

pred_regouttune <- predict(final_model, new_data = flights_valid) %>%
  bind_cols(flights_valid)

rmse_resultouttune <- yardstick::rmse(pred_regouttune, DEPARTURE_DELAY, .pred)
print(rmse_resultouttune)
rmse_resultouttune<- as.data.frame(rmse_resultouttune)
(RMSE_RT_OUT2<- rmse_resultouttune[ ,3])


# BENCH MARK PRETUNED AND TUNED REGRESSION TREES
TABLE_REGTREES<- as.table(matrix(c(RMSE_RT_IN, RMSE_RT_IN2, RMSE_RT_OUT, RMSE_RT_OUT2), ncol=2, byrow=TRUE))
colnames(TABLE_REGTREES) <- c("REGTREE", 'TUNED')
rownames(TABLE_REGTREES) <- c('RMSE IN', 'RMSE OUT')
TABLE_REGTREES

#### #### #### #### #### #### #### #### #### #### #### #### #### 
####  TREE BASED ENSEMBLE MODEL, XG BOOST####
#### #### #### #### #### #### #### #### #### #### #### #### #### 
library(tidymodels)
library(baguette) #FOR BAGGED TREES
library(ranger) #FOR RANDOM FORESTS
library(randomForest) #ALT RANDOM FOREST PACKAGE
library(xgboost) #FOR GRADIENT BOOSTING
library(caret) #FOR confusionMatrix()
library(vip) #FOR VARIABLE IMPORTANCE

ens_fmla<- DEPARTURE_DELAY ~ AIRLINE_DELAY + LATE_AIRCRAFT_DELAY + WEATHER_DELAY + AIR_SYSTEM_DELAY

boosted_forest <- boost_tree(min_n = NULL, #minimum number of observations for split
                             tree_depth = NULL, #max tree depth
                             trees = 100, #number of trees
                             mtry = NULL, #number of predictors selected at each split 
                             sample_size = NULL, #amount of data exposed to fitting
                             learn_rate = NULL, #learning rate for gradient descent
                             loss_reduction = NULL, #min loss reduction for further split
                             stop_iter = NULL)  %>% #maximum iteration for convergence
  set_engine("xgboost") %>%
  set_mode("regression") %>%
  fit(ens_fmla, flights_train)

pred_reg_gx_in <- predict(boosted_forest, new_data = flights_train) %>%
  bind_cols(flights_train)

# CALCULATE RMSE USING YARDSTICK
rmse_gxin <- yardstick::rmse(pred_reg_gx_in, DEPARTURE_DELAY, .pred)
print(rmse_gxin)
RMSE_GX_IN<- as.data.frame(rmse_gxin)
(RMSE_GX_IN<- RMSE_GX_IN[,3])

pred_reg_gx_out <- predict(boosted_forest, new_data = flights_valid) %>%
  bind_cols(flights_valid)

# CCALCULATE RMSE USING YARDSTICK
rmse_gxout <- yardstick::rmse(pred_reg_gx_out, DEPARTURE_DELAY, .pred)
print(rmse_gxout)

RMSE_GX_OUT<- as.data.frame(rmse_gxout)
(RMSE_GX_OUT<- RMSE_GX_OUT[,3])

# VIEW FEATURE IMPORTANCE RANKING
vip(boosted_forest)

library(rpart)
library(rpart.plot)

# PLOT GRADIENT BOOSTED REGRESSION TREE
gbxtreeplot <- rpart(ens_fmla, data = flights_train)
rpart.plot(gbxtreeplot, main = "Gradient Boosted Regression Tree")


# MODEL SELECTION           
TABLE_MVM<- as.table(matrix(c(RMSE_MM1_IN, RMSE_R4_IN, RMSE_5A_IN, RMSE_RT_IN2, RMSE_GX_IN, 
                              RMSE_MM1_OUT, RMSE_R4_OUT, RMSE_5A_OUT, RMSE_RT_OUT2, RMSE_GX_OUT), ncol=5, byrow=TRUE))
colnames(TABLE_MVM) <- c( "LINEAR", "RIDGE","SPLINE", 'TUNED', "GXBOOST")
rownames(TABLE_MVM) <- c('RMSE IN', 'RMSE OUT')
TABLE_MVM

#REPORT FINAL OUT OF SAMPLE ERROR             
ens_fmla<- DEPARTURE_DELAY ~ AIRLINE_DELAY + LATE_AIRCRAFT_DELAY + WEATHER_DELAY + AIR_SYSTEM_DELAY

boosted_forest_test <- boost_tree(min_n = NULL, #minimum number of observations for split
                             tree_depth = NULL, #max tree depth
                             trees = 100, #number of trees
                             mtry = NULL, #number of predictors selected at each split 
                             sample_size = NULL, #amount of data exposed to fitting
                             learn_rate = NULL, #learning rate for gradient descent
                             loss_reduction = NULL, #min loss reduction for further split
                             stop_iter = NULL)  %>% #maximum iteration for convergence
  set_engine("xgboost") %>%
  set_mode("regression") %>%
  fit(ens_fmla, flights_test)

# MAKE PREDICTIONS
pred_reg_gx_test <- predict(boosted_forest, new_data = flights_test) %>%
  bind_cols(flights_test) 

# CALCULATE RMSE USING YARDSTICK
rmse_gxtest <- yardstick::rmse(pred_reg_gx_test, DEPARTURE_DELAY, .pred)
print(rmse_gxtest)
RMSE_GX_TEST<- as.data.frame(rmse_gxtest)
(RMSE_GX_TEST<- RMSE_GX_TEST[,3])              
