#Import data
train <- read.csv("D:/ISEN 613/Project/Dataset 3 HAR/drive-download-20180424T041945Z-001/HartrainUncor.csv", header = TRUE, sep = ",")
test <- read.csv("D:/ISEN 613/Project/Dataset 3 HAR/drive-download-20180424T041945Z-001/HartestUncor.csv", header = TRUE, sep = ",")
HARdata <- read.csv("D:/ISEN 613/Project/Dataset 3 HAR/drive-download-20180424T041945Z-001/HarUncor.csv", header = TRUE, sep = ",")

dim(HARdata)
numPredictors=ncol(HARdata)#number of columns
numPredictors #number of predictors


str(HARdata)#for checking the structure of the dataframe
head(HARdata)#for checking top 5 rows of the dataframe

library(caret)
#10 fold cross validation repeated 3 times
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#Grid Cost parameter(C)= Penalty term. Higher the cost parameter, 
#better the classification but lesser is the smoothness of the boundary 
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(3233)

#training
##Second column is subject id, which cannot be a predictor
##The value of header 'Activity' changes to 'ï..Activity' while importing
svm_Linear_Grid <- train(ï..Activity ~., data = train[,-2], method = "svmLinear",trControl=trctrl,
                          preProcess = c("center", "scale"),tuneGrid = grid,tuneLength = 10)

svm_Linear_Grid
plot(svm_Linear_Grid)

#Testing
test_pred_grid <- predict(svm_Linear_Grid, newdata = test[,3:numPredictors])
##The value of header 'Activity' changes to 'ï..Activity' while importing
confusionMatrix(test_pred_grid, test$ï..Activity )