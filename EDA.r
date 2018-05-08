
library(ggplot2)
library(dendextend)
library(plyr)
library(reshape2)
library(lattice)
library(caret)
library(class)
library(MASS)
library(psych)

nsampleSize = -1L

train<-read.csv("C:/Users/Santosh/Desktop/Stat 489 Jupyter Notebook/613 project/Final Data/hartrain.csv",header= TRUE, sep=",")
test<-read.csv("C:/Users/Santosh/Desktop/Stat 489 Jupyter Notebook/613 project/Final Data/hartest.csv",header= TRUE, sep=",")

train <- transform(train, subjectID = factor(subject), activityID = factor(activity))
test <- transform(test, subjectID = factor(subject), activityID = factor(activity))

train$partition = "train"
test$partition = "test"

data <- rbind(train,test)

par(mfrow=c(1,2))
qplot(data= data,x= subjectID,fill= activityID) + theme(plot.margin= unit(c(1,1,1,1),"cm"))
qplot(data = data, x = subjectID, fill = partition) + theme(plot.margin= unit(c(1,1,1,1),"cm"))
par(mfrow=c(1,1))

sub1 <- subset(data, subjectID == 1)

distanceMatrix <- dist(sub1[, -c(562:564)])
hclustering <- hclust(distanceMatrix,, method='complete')
dend <- as.dendrogram(hclustering)
dend <- color_branches(dend, k = 6)
plot(dend)

numPredictors = ncol(data) - 5
dataSd = colwise(sd)(data[, 1:numPredictors])
dataSd$stat = "Predictor Variable Standard Deviation"
dataMean = colwise(mean)(data[, 1:numPredictors])
dataMean$stat = "Predictor Variable Mean"
temp = melt(rbind(dataMean, dataSd), c("stat"))
qplot(data = temp, x = value, binwidth = 0.025) + facet_wrap(~stat, ncol = 1)

numPredictors= ncol(train)-5
zScaleTrain = preProcess(train[, 1:numPredictors])
scaledX = predict(zScaleTrain, train[, 1:numPredictors])
correlatedPredictors = findCorrelation(cor(scaledX), cutoff = 0.8)
uncorData= scaledX[, -correlatedPredictors]
pcaTrain = preProcess(uncorData, method = "pca", thresh = 0.8)
pcaTrain

names(sub1)

n= ncol(sub1)-5
pr.out=prcomp(sub1[1:n], scale=TRUE)
pr.out$rotation

set.seed(45)
sub1Cluster <- kmeans(c(pr.out$x[,1],pr.out$x[,2]), 6, nstart = 5)
sub1clus <- sub1Cluster$cluster
par(mfrow=c(1,1))
plot(pr.out$x[,1],pr.out$x[,2], col= sub1clus,xlab='PC1',ylab='PC2')
plot(pr.out$x[,1],pr.out$x[,2], col= data$activityID,,xlab='PC1',ylab='PC2')
legend("topleft",legend= unique(sub1$activityID),col=unique(sub1$activityID),cex=0.6,pch=1)
