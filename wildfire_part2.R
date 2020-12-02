# Install packages
install.packages("visdat")
install.packages("corrplot")
install.packages("GGally")
install.packages("gridExtra")
install.packages("naniar")
install.packages("cvTools")
install.packages("glmnet")
install.packages("ISLR")
install.packages("boostrap")

# Load packages
library("ggplot2")
library("knitr")
library("visdat")
library("corrplot")
library("GGally")
library("gridExtra")
library("naniar")
library(dplyr)
library("cvTools")
library(tidyverse)
library(caret)
library(randomForest)
library(kernlab)
library(rpart)
library(neuralnet)
library("ISLR")
library("glmnet")
library(ggplot2)
library(mltools)

# Loading Data
data <- read.csv("/Users/thibautbadoual/Desktop/Aut/MS&E_226/MS&E_226_Project/Fire/FW_Veg_Rem_Combined.csv")
# data <- read.csv("~/Docs/Stanford/MSE226/Project/archive/FW_Veg_Rem_Combined.csv")

#----- Data Cleaning --------------------------------------------------------------------------#
# Remove unnecessary columns
data <- subset(data, select = -c(X, Unnamed..0, fire_name, fire_mag,
                                 cont_clean_date, disc_date_final, cont_date_final, 
                                 putout_time, disc_date_pre, disc_pre_month, 
                                 wstation_usaf, dstation_m, wstation_wban, 
                                 wstation_byear, wstation_eyear, weather_file, 
                                 Prec_cont, Hum_cont, Wind_cont, Temp_cont, 
                                 disc_clean_date, remoteness, disc_pre_year))

# Select only the top ten states
top_ten_states <- group_by(data, state) %>%
  summarise(count = n())
top_ten_states <- top_ten_states[order(-top_ten_states$count),]
top_ten_states <- top_ten_states$state[1:10]
data = filter(data, data$state %in% top_ten_states)

# Change vegetation type
data$Vegetation <- as.character(data$Vegetation)
data$Vegetation[which(data$Vegetation == "4")]  <- "Temperate Evergreen Needleleaf Forest" 
data$Vegetation[which(data$Vegetation == "9")]  <- "Grassland/Steppe"
data$Vegetation[which(data$Vegetation == "12")] <- "Open Shrubland"
data$Vegetation[which(data$Vegetation == "14")] <- "Desert"
data$Vegetation[which(data$Vegetation == "15")] <- "Polar Desert/Rock/Ice"
data$Vegetation[which(data$Vegetation == "16")] <- "Secondary Tropical Evergreen Broadleaf Forest"

# Remove duplicate rows
data <- data[!duplicated(data), ] 

# Drop missing values for vegetation data
data = filter(data, data$Vegetation != 0)

# Drop missing values for meteorological data
data = filter(data, data$Prec_pre_30 != -1.00000)
data = filter(data, data$Hum_pre_30 != 0)
data = filter(data, data$Hum_pre_15 != 0)
data = filter(data, data$Hum_pre_7 != 0)

# Add column with binary response variable (fire size class above C = 1; C or below = 0)
data_0 = filter(data, data$fire_size_class %in% c("A", "B", "C"))
data_1 = filter(data, data$fire_size_class %in% c("D", "E", "F", "G"))
data_0$binary_class <- 0
data_1$binary_class <- 1
data <- rbind(data_0, data_1)

#----- Data Overview --------------------------------------------------------------------------#
str(data) # Information on columns
summary(data) # Summary of columns
head(data) # Other info on dataset
glimpse(data)
dim(data)

#----- Data Transformations --------------------------------------------------------------------------#
# Add column with the seasons instead of the months
data_summer = filter(data, data$discovery_month %in% c("Aug", "Jul", "Sep"))
data_fall = filter(data, data$discovery_month %in% c("Nov", "Oct", "Dec"))
data_winter = filter(data, data$discovery_month %in% c("Feb", "Mar", "Jan"))
data_spring = filter(data, data$discovery_month %in% c("Apr", "May", "Jun"))

data_summer$season <- "summer"
data_fall$season <- "fall"
data_winter$season <- "winter"
data_spring$season <- "spring"
data_transformed <- rbind(data_summer, data_fall, data_winter, data_spring)

data_transformed$discovery_month <- NULL
data_transformed$latitude <- NULL

data_transformed$Prec_pre_30 <- log(data_transformed$Prec_pre_30 + 1)
data_transformed$Prec_pre_15 <- log(data_transformed$Prec_pre_15 + 1)
data_transformed$Prec_pre_7 <- log(data_transformed$Prec_pre_7 + 1)
data_transformed$Temp_pre_30 <- log(data_transformed$Temp_pre_30 + 373.15)
data_transformed$Temp_pre_15 <- log(data_transformed$Temp_pre_15 + 373.15)
data_transformed$Temp_pre_7 <- log(data_transformed$Temp_pre_7 + 373.15)

continuous_vars <- c("Prec_pre_30", "Prec_pre_15", "Prec_pre_7",
                     "Temp_pre_30", "Temp_pre_15", "Temp_pre_7",
                     "Wind_pre_30", "Wind_pre_15", "Wind_pre_7",
                     "Hum_pre_30", "Hum_pre_15", "Hum_pre_7")
factor_vars <- c("fire_size", "fire_size_class", "stat_cause_descr",
                 "longitude", "state", "Vegetation", "binary_class", "season")

data_normalized <- cbind(data_transformed[factor_vars], scale(data_transformed[continuous_vars]))

#----- Regression Predictions --------------------------------------------------------------------------#

# Set the train et test dataset
set.seed(1)

n = nrow(data); 
idx = sample(n, 0.8*n)

data_regression <- data
data_regression_normalized <- data_normalized

data_regression$binary_class <- NULL
data_regression$fire_size_class <- NULL
data_regression_normalized$binary_class <- NULL
data_regression_normalized$fire_size_class <- NULL

train_regression = data_regression[idx,]; 
train_regression_normalized = data_regression_normalized[idx,]; 
dim(train_regression)

test_regression = data_regression[-idx,];
test_regression_normalized = data_regression_normalized[-idx,];
dim(test_regression)

#correlation
ggcorr(data_regression, 
       method = c("all.obs","spearman"),
       nbreaks = 4, palette = 'RdBu', label = TRUE, 
       name = "spearman correlation coeff.(rho)",
       hjust = 0.8, angle = -70, size = 3) +
  ggtitle("Spearman Correlation coefficient Matrix")

glimpse(train_regression)
glimpse(test_regression)

#cross validation
ctrl_lm <- trainControl(method = "cv", number = 10)

#baseline model
lmCVFit <- train(fire_size~., data = train_regression_normalized, method = "lm", trControl = ctrl_lm, metric = "Rsquared")
print(lmCVFit)
summary(lmCVFit)

#final model
#train dataset
lmCVFit <- train(fire_size~. + Hum_pre_30:Wind_pre_30 + Hum_pre_15:Wind_pre_15 + Hum_pre_7:Wind_pre_7 
                 + Hum_pre_30:Temp_pre_30 + Hum_pre_15:Temp_pre_15 + Hum_pre_7:Temp_pre_7
                 , data = train_regression_normalized, method = "lm",
                 trControl = ctrl_lm, metric = "Rsquared")
print(lmCVFit)
summary(lmCVFit)

predictions <- predict(lmCVFit, newdata = test_regression_normalized, interval = "confidence")
rmse_test = rmse(test_regression_normalized$fire_size, predictions)

#test dataset
lmCVFit <- train(fire_size~. + Hum_pre_30:Wind_pre_30 + Hum_pre_15:Wind_pre_15 + Hum_pre_7:Wind_pre_7 
                 + Hum_pre_30:Temp_pre_30 + Hum_pre_15:Temp_pre_15 + Hum_pre_7:Temp_pre_7
                 , data = test_regression_normalized, method = "lm",
                 trControl = ctrl_lm, metric = "Rsquared")
print(lmCVFit)
summary(lmCVFit)

#----- Classification Predictions --------------------------------------------------------------------------#

data_predictions2 <- data

data_predictions2$fire_size <- NULL
data_predictions2$fire_size_class <- NULL

train2 = data_predictions2[idx,]; 
dim(train2)

test2 = data_predictions2[-idx,]; 
dim(test2)

#Baseline model / Train
fm.logisticR <- glm(binary_class ~ ., data=train2, family=binomial(link="logit"))
predicted_train <- predict(fm.logisticR, newdata = train2, type = "response")  # predicted scores

#Baseline model / Test
fm.logisticR <- glm(binary_class ~ ., data=test2, family=binomial(link="logit"))
predicted_test <- predict(fm.logisticR, newdata = test2, type = "response")  # predicted scores

# ROC curves
library(pROC)
ROC_train <- roc(train2$binary_class, predicted_train)
ROC_test <- roc(test2$binary_class, predicted_test)

# Review ROC objects
threshold_train <- coords(ROC_train, "best", "threshold")
threshold_test <- coords(ROC_test, "best", "threshold")

# Area Under Curve (AUC) for each ROC curve (higher -> better)
ROC_train_auc <- auc(ROC_train)
ROC_test_auc <- auc(ROC_test)

# plot ROC curves
plot(ROC_train, col = "green", main = "ROC: Train(green), Test(red) ")

points(ROC_test, col="red", pch="*")
lines(ROC_test, col="red")

# print the performance of each model
paste("Accuracy % Train Dataset: ", mean(train2$binary_class == round(predicted_train, digits = 0)))
paste("Accuracy % Test Dataset: ", mean(test2$binary_class == round(predicted_test, digits = 0)))
paste("Area under curve Train Dataset: ", ROC_train_auc)
paste("Area under curve Test Dataset: ", ROC_test_auc)
paste("Sensitivity Train Dataset: ", threshold_train[2])
paste("Sensitivity Test Dataset: ", threshold_test[2])


#----- Bootstrap --------------------------------------------------------------------------#

library("boot")

#cross validation
ctrl_lm <- trainControl(method = "cv", number = 10)

n_rep <- 5000

coeff.boot = function(data, indices){
  fm <- lm(data = data[indices,], fire_size~. + Hum_pre_30:Wind_pre_30 + Hum_pre_15:Wind_pre_15 + Hum_pre_7:Wind_pre_7 
                   + Hum_pre_30:Temp_pre_30 + Hum_pre_15:Temp_pre_15 + Hum_pre_7:Temp_pre_7)
  return(coef(fm))
}

boot.out = boot(train_regression_normalized, coeff.boot, n_rep)
boot.out

boot.ci(boot.out, type="norm", index=1)
boot.ci(boot.out, type="norm", index=2)
boot.ci(boot.out, type="norm", index=3)

boot.out$t0

plot(boot.out, index = 1)
