#Import data
train <- read.csv("D:/ISEN 613/Project/Dataset 3 HAR/drive-download-20180424T041945Z-001/HartrainUncor.csv", 
                  header = TRUE, sep = ",")
test <- read.csv("D:/ISEN 613/Project/Dataset 3 HAR/drive-download-20180424T041945Z-001/HartestUncor.csv", 
                 header = TRUE, sep = ",")
HARdata <- read.csv("D:/ISEN 613/Project/Dataset 3 HAR/drive-download-20180424T041945Z-001/HarUncor.csv", 
                    header = TRUE, sep = ",")

dim(HARdata)
numPredictors=ncol(HARdata)#number of columns
numPredictors #number of predictors

library(caret)
library(lattice)
#Standardize
##First column in the data is Response, second is subject id which cannot be a predictor
##Train
zScaleTrain = preProcess(train[, 3:numPredictors])
scaledXTrain = predict(zScaleTrain, train[, 3:numPredictors])
##Test
zScaleTest = preProcess(test[, 3:numPredictors])
scaledXTest = predict(zScaleTest, test[, 3:numPredictors])
##Total
zScaleHARdata = preProcess(HARdata[, 3:numPredictors])
scaledXHARdata = predict(zScaleHARdata, HARdata[, 3:numPredictors])

dim(scaledXTrain)
n=ncol(scaledXTrain)#number of columns
n #number of pure predictors is n

#PCA
pr.out=prcomp(scaledXTrain, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation

biplot(pr.out,scale=0)
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var

pve=pr.var/sum(pr.var)
sum=0
pve
#cumulative sum of variance ratios (PVE)
for (i in 1:n)
{
  if(i==1)
  {sum[1]=pve[1]}
  else
  {sum[i]=sum[i-1]+pve[i]}
}
sum[1:n]

#computing PVE indices for different % of variances
varlim=0.8#80% fraction of total variance to be explained
j=0
for (i in 2:n-1)
{
  if(sum[i]>=varlim && sum[i-1]<varlim)
  {j=i}#index of the PC till which 80% of the variance is explained
  else
  {j=j}
}
j

#Linear SVM
##Dimension rotation
##Second column is subject id, which cannot be a predictor
##The value of header 'Activity' changes to 'ï..Activity' while importing
svm.train = predict(pr.out,train[,3:numPredictors])[,1:j]
svm.train.mat = cbind(svm.train, train$ï..Activity) 

write.csv(svm.train.mat, file = 'svmtrain.csv')
svm.tr=read.csv('svmtrain.csv', header = TRUE, sep = ","  )
svm.tr=svm.tr[,-1]#First column is the row serial number while writing csv

#Linear
##Train
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)#K fold Cross Validation
set.seed(3233)

svm_Linear <- train(as.factor(X.1) ~., data = svm.tr, method = "svmLinear",trControl=trctrl, tuneLength = 10)

##Test
##Dimension rotation for test
tst = test[,3:numPredictors]
dim(tst)
svm.test = predict(pr.out,tst)[,1:j]
write.csv(svm.test, file = 'svmtest.csv')
svm.test=read.csv('svmtest.csv', header = TRUE, sep = ","  )
svm.test=svm.test[,-1]
predsvm = predict(svm_Linear,svm.test)
predsvm

ressvm = table(predsvm,test$ï..Activity)#predsvm does not have headers
ressvm #Confusion matrix
ressvmpc = ressvm
k=0; m=0
for (i in 1:6) {
  for (j in 1:6)
  {
    ressvmpc[i,j] = round(ressvmpc[i,j]/sum(ressvm[,j])*100,2)
    if (i==j)
    {k=k+ressvm[i,j]}
    m=m+ressvm[i,j]
  }
}
ressvmpc #Confusion matrix in percentages
accsvm=k/m*100
print(accsvm)
write.csv(ressvmpc, file = 'ressvmpc.csv')


