# Load required libraries
library(tidyverse) 
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
library(ggplot2)

# Set working directory and load data
setwd("D:/Study/Semester 8/Distributed Computing/Labs/Labs/Project_Data/Predicting Housing Price")
rm(list = ls())
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Combine training and test data into one dataframe without SalePrice Column
Data <- rbind(train[,0:80], test)


# remove Id column
Data <- Data[,-1]
# Convert non-numeric columns to factors and then to numeric
for (i in 1:ncol(Data)) {
  if (!is.numeric(Data[[i]])) {
    Data[[i]] <- as.numeric(as.factor(Data[[i]]))
    
  }
}


# Check for missing values
print(paste0("Number of null values = ",sum(is.na(Data))))
print("Columns have null values are")
#Columns that Have Null Values
names(Data)[colSums(is.na(Data)) > 0]
# replace missing values with median
for (i in 1:ncol(Data)) {
  Data[[i]] <- ifelse(is.na(Data[[i]]), median(Data[[i]],na.rm = TRUE), Data[[i]])
}



print(paste0("Number of null values = ",sum(is.na(Data))))

#creating new Features
Data$Age<-Data$YrSold-Data$YearBuilt
#TotalSF--> total square feet
#TotalBsmtSF-->Total square feet of basement area
Data$TotalSF<-Data$X1stFlrSF+Data$X2ndFlrSF+Data$TotalBsmtSF
Data$TotalBath<-Data$FullBath+Data$HalfBath
Data$TotalPorchSF<-Data$WoodDeckSF+Data$OpenPorchSF+Data$EnclosedPorch+Data$X3SsnPorch+Data$ScreenPorch

Data <- subset(Data, select = -c(YrSold, YearBuilt,
                                 X1stFlrSF,X2ndFlrSF,TotalBsmtSF,
                                 FullBath,HalfBath,
                                 WoodDeckSF,OpenPorchSF,EnclosedPorch,X3SsnPorch,ScreenPorch))



# Normalize the data
Data <- as.data.frame(scale(Data))

# Split the data into train and test sets
ntrain <- Data[0:1460,]
ntest <- Data[1461:2919,]

#add SalePrice Column to train data
ntrain$SalePrice<- train$SalePrice


# Select features with high correlation with SalePrice
corr_saleprice <-cor(ntrain$SalePrice,ntrain[,0:72])
print(corr_saleprice)
vec <- unlist(as.data.frame(corr_saleprice))
high_corr <- subset(vec, abs(vec) >= 0.35)
high_corr
print("Selected Features are")
names(high_corr)

# Data visualization
library(ggplot2)
library(reshape2)
# Assuming you have the House Prices dataset loaded as a data frame called 'house_prices'
# Replace 'house_prices' with the actual variable storing the dataset

# Visualize the distribution of SalePrice using a histogram
ggplot(ntrain, aes(x = SalePrice)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(x = 'Sale Price', y = 'Frequency', title = 'Distribution of Sale Prices') +
  theme_minimal()

# Create a box plot to compare SalePrice across different OverallQual categories
ggplot(ntrain, aes(x = factor(OverallQual), y = SalePrice)) +
  geom_boxplot(fill = 'steelblue', color = 'black') +
  labs(x = 'Overall Quality', y = 'Sale Price', title = 'Sale Price across Overall Quality') +
  theme_minimal()

# Create a bar plot to visualize the count of houses in each Neighborhood
ggplot(ntrain, aes(x = Neighborhood)) +
  geom_bar(fill = 'steelblue', color = 'black') +
  labs(x = 'Neighborhood', y = 'Count', title = 'Count of Houses in Each Neighborhood') +
  theme_minimal()

ggplot(data = ntrain, aes(x = ntrain$OverallQual, y =SalePrice )) +
  geom_point() +
  xlab ("Overall Quality")+
  ylab ("SalePrice") +
  ggtitle("Overall Quality vs SalePrice")

ggplot(data = ntrain, aes(x = ntrain$GrLivArea, y = SalePrice)) +
  geom_point() +
  ylab("SalePrice") +
  xlab("Above Ground Living Area") +
  ggtitle("Above Ground Living Area vs SalePrice")
###############################################################################################
# YearRemodAdd - Histogram
ggplot(ntrain, aes(x = YearRemodAdd)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(x = 'Year Remodeled', y = 'Frequency', title = 'Distribution of Year Remodeled') +
  theme_minimal()

# MasVnrArea - Histogram
ggplot(ntrain, aes(x = MasVnrArea)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(x = 'Masonry Veneer Area', y = 'Frequency', title = 'Distribution of Masonry Veneer Area') +
  theme_minimal()

# ExterQual - Bar plot
ggplot(ntrain, aes(x = ExterQual)) +
  geom_bar(fill = 'steelblue', color = 'black') +
  labs(x = 'Exterior Quality', y = 'Count', title = 'Distribution of Exterior Quality') +
  theme_minimal()

# Foundation - Bar plot
ggplot(ntrain, aes(x = Foundation)) +
  geom_bar(fill = 'steelblue', color = 'black') +
  labs(x = 'Foundation Type', y = 'Count', title = 'Distribution of Foundation Type') +
  theme_minimal()

# BsmtQual - Bar plot
ggplot(ntrain, aes(x = BsmtQual)) +
  geom_bar(fill = 'steelblue', color = 'black') +
  labs(x = 'Basement Quality', y = 'Count', title = 'Distribution of Basement Quality') +
  theme_minimal()

# BsmtFinSF1 - Histogram
ggplot(ntrain, aes(x = BsmtFinSF1)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(x = 'Basement Finished Area', y = 'Frequency', title = 'Distribution of Basement Finished Area') +
  theme_minimal()

# HeatingQC - Bar plot
ggplot(ntrain, aes(x = HeatingQC)) +
  geom_bar(fill = 'steelblue', color = 'black') +
  labs(x = 'Heating Quality', y = 'Count', title = 'Distribution of Heating Quality') +
  theme_minimal()

# GrLivArea - Histogram
ggplot(ntrain, aes(x = GrLivArea)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(x = 'Above Grade Living Area', y = 'Frequency', title = 'Distribution of Above Grade Living Area') +
  theme_minimal()

# KitchenQual - Bar plot
ggplot(ntrain, aes(x = KitchenQual)) +
  geom_bar(fill = 'steelblue', color = 'black') +
  labs(x = 'Kitchen Quality', y = 'Count', title = 'Distribution of Kitchen Quality') +
  theme_minimal()

# TotRmsAbvGrd - Bar plot
ggplot(ntrain, aes(x = factor(TotRmsAbvGrd))) +
  geom_bar(fill = 'steelblue', color = 'black') +
  labs(x = 'Total Rooms Above Grade', y = 'Count')
###############################################################################################

barplot(high_corr, names.arg = names(high_corr), xlab = "", ylab = "SalePrice", main = "high Correlation Features",las=2)
# Drop the Features with low correlation
ntrain<-subset(ntrain,select = names(high_corr))
hig_corr <- high_corr[names(high_corr) != "SalePrice"]
ntest<-subset(ntest,select = names(hig_corr))

# library(lattice)
# splom(~ntest,groups = NULL,data = ntest)
############################################################################ MODELS ###############################################################################
# # Train a logistic regression model
# model <- glm(log(SalePrice) ~.,data = ntrain)
# # Make predictions on test data
# predictions <- predict(model, newdata = ntest)
# train_predictions <- predict(model, newdata = ntrain[,-ncol(ntrain)])
# 
# # Calculate RMSE on train data
# rmse <- RMSE(train_predictions, log(ntrain$SalePrice))
# print(paste0("RMSE: ", round(rmse,3)))

###################################################################################################################################################################
# # Train a decision tree model
# model <- rpart(log(SalePrice) ~.,data = ntrain,method = "anova")
# # Plot decision tree
# rpart.plot(model, main = "Regression Tree")
# # Make predictions on test data
# predictions <- predict(model, newdata = ntest)
# train_predictions <- predict(model, newdata = ntrain[,-ncol(ntrain)])
# 
# # Calculate RMSE on train data
# rmse <- RMSE(train_predictions, log(ntrain$SalePrice))
# print(paste0("RMSE: ", round(rmse,3)))
###################################################################################################################################################################
# Train a SVM model
# model <- svm(log(SalePrice) ~.,data = ntrain,kernel = "linear", cost = 1, gamma = 1)
# # Make predictions on test data
# predictions <- predict(model, newdata = ntest)
# train_predictions <- predict(model, newdata = ntrain[,-ncol(ntrain)])
# 
# # Calculate RMSE on train data
# rmse <- RMSE(train_predictions,log(ntrain$SalePrice))
# print(paste0("RMSE: ", round(rmse,3)))
###################################################################################################################################################################
# Train a random forest regression model
model <- randomForest(SalePrice ~.,data = ntrain, ntree = 500,mtry = 3,nodesize = 1,importance = TRUE,na.action = na.pass)
# Make predictions on test data
predictions <- predict(model, newdata = ntest)
train_predictions <- predict(model, newdata = ntrain[,-ncol(ntrain)])

# Calculate RMSE on train data
rmse <- RMSE(train_predictions, ntrain$SalePrice)
print(paste0("RMSE: ", round(rmse,3)))

#Visualize Model predictions
# Create a data frame with actual and predicted values
prediction_df <- data.frame(Actual = ntrain$SalePrice, Predicted = train_predictions)
# Create a scatter plot of actual vs. predicted values
ggplot(prediction_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual", y = "Predicted", title = "Random Forest Model Predictions")
###################################################################################################################################################################

# Save predictions to a csv file
ID <-test$Id
results <- data.frame(ID, predictions)
colnames(results) <- c("Id","SalePrice")
write.csv(results, file = "results.csv", row.names = FALSE)

