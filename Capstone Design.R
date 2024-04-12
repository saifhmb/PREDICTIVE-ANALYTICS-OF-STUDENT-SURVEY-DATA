# Capstone Design
# Exploring the dataset
library(readxl)
Dataset <- read_excel("DATASET.xlsx")
str(Dataset)
summary(Dataset)
nrow(Dataset)
ncol(Dataset)
# Checking for missing data
sum(is.na(Dataset))
# Removing student ID and course ID column because it is just an identifier and not a predictor
Dataset <- Dataset[-1]
Dataset <- Dataset[-31]

# Data Preprocessing
Dataset$GRADE <- factor(Dataset$GRADE)
## Train and test data preparation
library(caret)
set.seed(123)
#70-30 training-test split
Dataset_sampling_vector <- createDataPartition(Dataset$GRADE, p = 0.7, list = FALSE)
Dataset_train <- Dataset[Dataset_sampling_vector,]
Dataset_test <- Dataset[-Dataset_sampling_vector,]
## Normalize input data
Dataset_pp <- preProcess(Dataset_train[1:30], method =c("range"))
Dataset_train <- cbind(predict(Dataset_pp, Dataset_train[1:30]), GRADE = Dataset_train$GRADE)
Dataset_test <- cbind(predict(Dataset_pp, Dataset_test[1:30]), GRADE = Dataset_test$GRADE)
head(Dataset_train)
#80-20 training-test split
Dataset_sampling_vector1 <- createDataPartition(Dataset$GRADE, p = 0.8, list = FALSE)
Dataset_train1 <- Dataset[Dataset_sampling_vector1,]
Dataset_test1 <- Dataset[-Dataset_sampling_vector1,]
## Normalize input data
Dataset_pp1 <- preProcess(Dataset_train1[1:30], method =c("range"))
Dataset_train1 <- cbind(predict(Dataset_pp1, Dataset_train1[1:30]), GRADE = Dataset_train1$GRADE)
Dataset_test1 <- cbind(predict(Dataset_pp1, Dataset_test1[1:30]), GRADE = Dataset_test1$GRADE)
head(Dataset_train1)
#85-15 training-test split
Dataset_sampling_vector2 <- createDataPartition(Dataset$GRADE, p = 0.85, list = FALSE)
Dataset_train2 <- Dataset[Dataset_sampling_vector2,]
Dataset_test2 <- Dataset[-Dataset_sampling_vector2,]
## Normalize input data
Dataset_pp2 <- preProcess(Dataset_train2[1:30], method =c("range"))
Dataset_train2 <- cbind(predict(Dataset_pp2, Dataset_train2[1:30]), GRADE = Dataset_train2$GRADE)
Dataset_test2 <- cbind(predict(Dataset_pp2, Dataset_test2[1:30]), GRADE = Dataset_test2$GRADE)
head(Dataset_train2)

# Modeling 
library(nnet)
#70-30 model
Dataset_Model <- nnet(GRADE ~ ., data = Dataset_train, size = 22)
#80-20 model
Dataset_Model1 <- nnet(GRADE ~ ., data = Dataset_train1, size = 22)
#85-15 model
Dataset_Model2 <- nnet(GRADE ~ ., data = Dataset_train2, size = 22)
## Rule of thumb for hidden layer should be between two-third  to twice the number of nodes in the input layer. Here we choose 22.

#Evaluation - training set
train_predictions <- predict(Dataset_Model, Dataset_train[,1:30], type = "class")
mean(train_predictions == Dataset_train$GRADE)
train_predictions1 <- predict(Dataset_Model1, Dataset_train1[,1:30], type = "class")
mean(train_predictions1 == Dataset_train1$GRADE)
train_predictions2 <- predict(Dataset_Model2, Dataset_train2[,1:30], type = "class")
mean(train_predictions2 == Dataset_train2$GRADE)

# Evaluation test set
#70-30
test_predictions <- predict(Dataset_Model, Dataset_test[,1:30], type = "class")
mean(test_predictions == Dataset_test$GRADE)
#80-20
test_predictions1 <- predict(Dataset_Model1, Dataset_test1[,1:30], type = "class")
mean(test_predictions1 == Dataset_test1$GRADE)
#85-15
test_predictions2 <- predict(Dataset_Model2, Dataset_test2[,1:30], type = "class")
mean(test_predictions2 == Dataset_test2$GRADE)

# SAVE FOR FINAL REP: Even though our model fits the training data perfectly, we see that the accuracy on the test set is 
#only 60 percent. Applying decay to restrict overfitting on the training data yields
#Dataset_ModelA <- nnet(GRADE ~ ., data = Dataset_train, size = 22, decay = .01, maxit = 1000)
#Dataset_ModelA1 <- nnet(GRADE ~ ., data = Dataset_train1, size = 22, decay = .01, maxit = 1000)
#Dataset_ModelA2 <- nnet(GRADE ~ ., data = Dataset_train2, size = 22, decay = .01, maxit = 1000)
#Evaluation - training set
#70-30
#train_predictionsA <- predict(Dataset_ModelA, Dataset_train[,1:31], type = "class")
#mean(train_predictionsA == Dataset_train$GRADE)
#80-20
#train_predictionsA1 <- predict(Dataset_ModelA1, Dataset_train1[,1:31], type = "class")
#mean(train_predictionsA1 == Dataset_train1$GRADE)
#85-15
#train_predictionsA2 <- predict(Dataset_ModelA2, Dataset_train2[,1:31], type = "class")
#mean(train_predictionsA2 == Dataset_train2$GRADE)

# Evaluation test set
#70-30
#test_predictionsA <- predict(Dataset_ModelA, Dataset_test[,1:31], type = "class")
#mean(test_predictionsA == Dataset_test$GRADE)
#80-20
#test_predictionsA1 <- predict(Dataset_ModelA1, Dataset_test1[,1:31], type = "class")
#mean(test_predictionsA1 == Dataset_test1$GRADE)
#85-15
#test_predictionsA2 <- predict(Dataset_ModelA2, Dataset_test2[,1:31], type = "class")
#mean(test_predictionsA2 == Dataset_test2$GRADE)

#library(caret)
#nnet_grid <- expand.grid(.decay = c(0.1), .size = c(32))
#nnetfit <- train(GRADE ~ ., data = Dataset_train, method = "nnet",
                   #maxit = 10000, tuneGrid = nnet_grid, trace = F, MaxNWts = 10000)

# Variable importance factor for 70-30
Dataset_Model_varImp <- varImp(Dataset_Model)
Dataset_Model_varImp <- as.data.frame(Dataset_Model_varImp)
Dataset_Model_varImp <- Dataset_Model_varImp[1]
Dataset_Model_varImp[order(-Dataset_Model_varImp$Overall), , drop = FALSE]

sum(Dataset_Model_varImp$Overall)

# Variable importance factor for 80-20
Dataset_Model1_varImp <- varImp(Dataset_Model1)
Dataset_Model1_varImp <- as.data.frame(Dataset_Model1_varImp)
Dataset_Model1_varImp <- Dataset_Model1_varImp[1]
Dataset_Model1_varImp[order(-Dataset_Model1_varImp$Overall), , drop = FALSE]

sum(Dataset_Model1_varImp$Overall)

# Variable importance factor for 85-15
Dataset_Model2_varImp <- varImp(Dataset_Model2)
Dataset_Model2_varImp <- as.data.frame(Dataset_Model2_varImp)
Dataset_Model2_varImp <- Dataset_Model2_varImp[1]
Dataset_Model2_varImp[order(-Dataset_Model2_varImp$Overall), , drop = FALSE]

sum(Dataset_Model2_varImp$Overall)

# Multiple Linear Regression
Dataset <- read_excel("DATASET.xlsx")
Dataset <- Dataset[-1]
Dataset <- Dataset[-31]
LinearRegressionModel<-lm(GRADE~ ., Dataset) # Use the rest variable to predict CLAIM
LinearRegressionModel
summary(LinearRegressionModel)
# Optimization for feature selection
OptLM <- step(LinearRegressionModel)
summary(OptLM)
