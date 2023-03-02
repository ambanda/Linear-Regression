
library(tidyverse)
library(dplyr)
library(caret)
library(Metrics)

library(devtools)

install.packages("chemometrics") #if this fails try installing previous version of chemometrics by below install_version() function from the devtools package:


install_version("chemometrics", version = "1.4", repos = "http://cran.us.r-project.org")

library(chemometrics)

#Question a

# Load the tecator dataset

data(tecator)
??tecator
print(tecator)

# Display information about the tecator dataset
str(tecator)
class(tecator)

data(package = "chemometrics") # use this function to discover if tecator dataset exists in chemometrics that loaded.


#Question b


# Perform PCA on the absorp matrix
pca <- prcomp(absorp)

# Plot the scree plot
plot(pca)

# Determine the effective dimension based on the scree plot
scree <- pca$sdev^2 / sum(pca$sdev^2)
effective_dimension <- sum(cumsum(scree) <= 0.95)
cat("The effective dimension is", effective_dimension)

'''the effective dimension is the number of principal components needed to explain at least 95% of the variance. 
Based on the scree plot, it appears that the effective dimension is approximately 25'''


#QUestion c

# Split the data into a training and a test set
set.seed(123)
trainIndex <- createDataPartition(tecator$Moisture, p = 0.8, list = FALSE)
trainData <- tecator[trainIndex, ]
testData <- tecator[-trainIndex, ]

# Pre-process the data
preProc <- preProcess(trainData, method = c("center", "scale"))
trainDataTransformed <- predict(preProc, trainData)
testDataTransformed <- predict(preProc, testData)

# Build linear regression model
lmFit <- train(Moisture ~ ., data = trainDataTransformed, method = "lm")

# Build partial least squares regression model
plsFit <- train(Moisture ~ ., data = trainDataTransformed, method = "pls")

# Build elastic net model
enetFit <- train(Moisture ~ ., data = trainDataTransformed, method = "glmnet")

# Build support vector regression model
svmFit <- train(Moisture ~ ., data = trainDataTransformed, method = "svmRadial")

# Build random forest regression model
rfFit <- train(Moisture ~ ., data = trainDataTransformed, method = "rf")

# Display the optimal tuning parameters for each model
print(lmFit)
print(plsFit)
print(enetFit)
print(svmFit)
print(rfFit)


#Question d

# Make predictions on the test set using each model
lmPred <- predict(lmFit, newdata = testDataTransformed)
plsPred <- predict(plsFit, newdata = testDataTransformed)
enetPred <- predict(enetFit, newdata = testDataTransformed)
svmPred <- predict(svmFit, newdata = testDataTransformed)
rfPred <- predict(rfFit, newdata = testDataTransformed)

# Evaluate the performance of each model on the test set

lmRMSE <- rmse(testData$Moisture, lmPred)
plsRMSE <- rmse(testData$Moisture, plsPred)
enetRMSE <- rmse(testData$Moisture, enetPred)
svmRMSE <- rmse(testData$Moisture, svmPred)
rfRMSE <- rmse(testData$Moisture, rfPred)

'''Based on the RMSE values, we can see that the random forest regression model has the lowest RMSE and therefore the best predictive ability on the test set
'''
#Question e

# Split the data into a training and a test set for Fat
set.seed(123)
trainIndex_fat <- createDataPartition(tecator$Fat, p = 0.8, list = FALSE)
trainData_fat <- tecator[trainIndex_fat, ]
testData_fat <- tecator[-trainIndex_fat, ]

# Pre-process the data
preProc <- preProcess(trainData_fat, method = c("center", "scale"))
trainDataTransformed_fat <- predict(preProc, trainData_fat)
testDataTransformed_fat <- predict(preProc, testData_fat)


# Build linear regression model
lmFit_fat <- train(Fat ~ ., data = trainDataTransformed_fat, method = "lm")

# Build partial least squares regression model
plsFit_fat <- train(Fat ~ ., data = trainDataTransformed_fat, method = "pls")

# Build elastic net model
enetFit_fat <- train(Fat ~ ., data = trainDataTransformed_fat, method = "glmnet")

# Build support vector regression model
svmFit_fat <- train(Fat ~ ., data = trainDataTransformed_fat, method = "svmRadial")

# Build random forest regression model
rfFit_fat <- train(Fat ~ ., data = trainDataTransformed_fat, method = "rf")

# Display the optimal tuning parameters for each model
print(lmFit_fat)
print(plsFit_fat)
print(enetFit_fat)
print(svmFit_fat)
print(rfFit_fat)

# Make predictions on the test set using each model
lmPred <- predict(lmFit_fat, newdata = testDataTransformed_fat)
plsPred <- predict(plsFit_fat, newdata = testDataTransformed_fat)
enetPred <- predict(enetFit_fat, newdata = testDataTransformed_fat)
svmPred <- predict(svmFit_fat, newdata = testDataTransformed_fat)
rfPred <- predict(rfFit_fat, newdata = testDataTransformed_fat)

# Evaluate the performance of each model on the test set

lmRMSE_fat <- rmse(testData_fat$Fat, lmPred)
plsRMSE_fat <- rmse(testData_fat$Fat, plsPred)
enetRMSE_fat <- rmse(testData_fat$Fat, enetPred)
svmRMSE_fat <- rmse(testData_fat$Fat, svmPred)
rfRMSE_fat <- rmse(testData_fat$Fat, rfPred)

'''we can say that the random forest model performs better than the other models since it has lower RSME.'''


