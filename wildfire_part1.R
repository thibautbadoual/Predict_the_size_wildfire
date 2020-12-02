# Install packages
install.packages("visdat")
install.packages("corrplot")
install.packages("GGally")
install.packages("gridExtra")
install.packages("naniar")
install.packages("cvTools")
install.packages("glmnet")
install.packages("ISLR")

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

# Loading Data
# data <- read.csv("/Users/thibautbadoual/Desktop/MS&E_226_Project/Fire/archive/FW_Veg_Rem_Combined.csv")
data <- read.csv("~/Docs/Stanford/MSE226/Project/archive/FW_Veg_Rem_Combined.csv")

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

#----- Plots --------------------------------------------------------------------------#
# Plots
glimpse(data)

# Number of fires per wildfire class
ggplot(data, aes(fire_size_class)) + geom_histogram(stat = "count") +
  xlab("Wildfire Class (A-G)") + ylab("Number of fire for each class")

# Longitude and Latitude vs fire size class
p1 <- ggplot(data, aes(fire_size_class, longitude)) + geom_boxplot()
p2 <- ggplot(data, aes(fire_size_class, latitude)) + geom_boxplot()
grid.arrange(p1, p2, nrow = 1)

p3 <- ggplot(data, aes(as.factor(discovery_month), Temp_pre_30)) + geom_boxplot() + scale_x_discrete(limits = month.abb)
p4 <- ggplot(data, aes(as.factor(disc_pre_year), Temp_pre_30)) + geom_boxplot()
grid.arrange(p3, p4, nrow = 1)

p5<- ggplot(data, aes(as.factor(discovery_month), Wind_pre_30)) + geom_boxplot() + scale_x_discrete(limits = month.abb)
p6 <- ggplot(data, aes(as.factor(disc_pre_year), Wind_pre_30)) + geom_boxplot()
grid.arrange(p5, p6, nrow = 1)

p7<- ggplot(data, aes(as.factor(discovery_month), Prec_pre_30)) + geom_boxplot() + scale_x_discrete(limits = month.abb)
p8 <- ggplot(data, aes(as.factor(disc_pre_year), Prec_pre_30)) + geom_boxplot()
grid.arrange(p7, p8, nrow = 1)

p9<- ggplot(data, aes(as.factor(discovery_month), Hum_pre_30)) + geom_boxplot() + scale_x_discrete(limits = month.abb)
p10 <- ggplot(data, aes(as.factor(disc_pre_year), Hum_pre_30)) + geom_boxplot()
grid.arrange(p9, p10, nrow = 1)

p11 <- ggplot(data, aes(fire_size_class, Hum_pre_30)) + geom_boxplot()
p12 <- ggplot(data, aes(fire_size_class, Prec_pre_30)) + geom_boxplot()
p13 <- ggplot(data, aes(fire_size_class, Wind_pre_30)) + geom_boxplot()
p14 <- ggplot(data, aes(fire_size_class, Temp_pre_30)) + geom_boxplot()
grid.arrange(p11, p12, p13, p14, nrow = 2)

p15 <- ggplot(data, aes(Temp_pre_30)) + geom_histogram(bins = 50)
p16 <- ggplot(data, aes(Prec_pre_30)) + geom_histogram(bins = 50)
p17 <- ggplot(data, aes(Hum_pre_30)) + geom_histogram(bins = 50)
p18 <- ggplot(data, aes(Wind_pre_30)) + geom_histogram(bins = 50)
grid.arrange(p15, p16, p17, p18, nrow = 2)

# Cause of Fire 
count_causes_data <- group_by(data, binary_class, stat_cause_descr) %>%
  summarise(count = n())
p19 <- ggplot(count_causes_data, aes(x = stat_cause_descr, y = count)) + geom_bar(position="dodge", stat="identity")
p20 <- ggplot(count_causes_data, aes(x = stat_cause_descr, y = count, fill = binary_class)) + geom_bar(stat="identity")
grid.arrange(p19, nrow = 1)
grid.arrange(p20, nrow = 1)

# State
count_state <- group_by(data, binary_class, state) %>%
  summarise(count = n())
p21 <- ggplot(count_state, aes(x = state, y = count)) + geom_bar(position="dodge", stat="identity")
p22 <- ggplot(count_state, aes(x = state, y = count, fill = binary_class)) + geom_bar(stat="identity")
grid.arrange(p21, nrow = 1)
grid.arrange(p22, nrow = 1)

# Vegetation
count_vegetation <- group_by(data, binary_class, Vegetation) %>%
  summarise(count = n())
p23 <- ggplot(count_vegetation, aes(x = Vegetation, y = count, fill = binary_class)) + geom_bar(position="dodge", stat="identity")
p24 <- ggplot(count_vegetation, aes(x = Vegetation, y = count, fill = binary_class)) + geom_bar(stat="identity")
grid.arrange(p23, nrow = 1)
grid.arrange(p24, nrow = 1)

#######BONUS#######
plot(data$fire_size, data$Hum_pre_30)
plot(data$fire_size, data$Temp_pre_30)
plot(data$fire_size, data$Wind_pre_30)
plot(data$fire_size, data$Prec_pre_30)

plot(data$fire_size, data$longitude)
plot(data$fire_size, data$latitude)

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

#Baseline 
lmCVFit <- train(fire_size~., data = train_regression, method = "lm",
                 trControl = ctrl_lm, metric = "Rsquared")
print(lmCVFit)

#model 1 / all interactions
lmCVFit <- train(fire_size~.+.:., data = train_regression, method = "lm",
                 trControl = ctrl_lm, metric = "Rsquared")
print(lmCVFit)

#model 2 / all transformations
lmCVFit <- train(fire_size~., 
                 data = train_regression_normalized, method = "lm",
                 trControl = ctrl_lm, metric = "Rsquared")
print(lmCVFit)

#model 3 / all relevant interactions
lmCVFit <- train(fire_size~. + Hum_pre_30:Wind_pre_30 + Hum_pre_15:Wind_pre_15 + Hum_pre_7:Wind_pre_7 
                 + Hum_pre_30:Temp_pre_30 + Hum_pre_15:Temp_pre_15 + Hum_pre_7:Temp_pre_7
                 , data = train_regression, method = "lm",
                 trControl = ctrl_lm, metric = "Rsquared")
print(lmCVFit)

#final model / all combined
lmCVFit <- train(fire_size~. + Hum_pre_30:Wind_pre_30 + Hum_pre_15:Wind_pre_15 + Hum_pre_7:Wind_pre_7 
                 + Hum_pre_30:Temp_pre_30 + Hum_pre_15:Temp_pre_15 + Hum_pre_7:Temp_pre_7
                 , data = train_regression_normalized, method = "lm",
                 trControl = ctrl_lm, metric = "Rsquared")
print(lmCVFit)

#-------------------------------------------------------------------------------#

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

#-------------------------------------------------------------------------------#

#Lasso (alpha = 1) and Ridge (alpha = 0) Regression
X = model.matrix(fire_size ~ 0 + ., train_regression_normalized)
Y = train_regression_normalized$fire_size

train.ind = sample(nrow(X), round(nrow(X)/2)) 
X.train = X[train.ind,]
X.test = X[-train.ind,]
Y.train = Y[train.ind]
Y.test = Y[-train.ind]

lambdas = 10^seq(-2,3.4,0.1)

# Setting alpha = 0 implements ridge regression
fm.ridge <- cv.glmnet(X.train, Y.train, alpha = 0, lambda = lambdas, thresh = 1e-12)
optimal_lambda <- fm.ridge$lambda.min

# Prediction and evaluation on train data
predictions_train <- predict(fm.ridge, s = optimal_lambda, newx = X.train)
eval_results(Y.train, predictions_train, X.train)

# Prediction and evaluation on test data
predictions_test <- predict(fm.ridge, s = optimal_lambda, newx = X.test)
eval_results(Y.test, predictions_test, X.test)


# Setting alpha = 1 implements lasso regression
fm.lasso <- cv.glmnet(X.train, Y.train, alpha = 1, lambda = lambdas, thresh = 1e-12)
optimal_lambda <- fm.lasso$lambda.min

# Prediction and evaluation on train data
predictions_train <- predict(fm.lasso, s = optimal_lambda, newx = X.train)
eval_results(Y.train, predictions_train, X.train)

# Prediction and evaluation on test data
predictions_test <- predict(fm.lasso, s = optimal_lambda, newx = X.test)
eval_results(Y.test, predictions_test, X.test)

#----- Classification Predictions --------------------------------------------------------------------------#

data_predictions2 <- data

data_predictions2$fire_size <- NULL
data_predictions2$fire_size_class <- NULL

train2 = data_predictions2[idx,]; 
dim(train2)

test2 = data_predictions2[-idx,]; 
dim(test2)

#Baseline model
fm.logisticR <- glm(binary_class ~ ., data=train2, family=binomial(link="logit"))
predicted2 <- predict(fm.logisticR, newdata = train2, type = "response")  # predicted scores

