library(glmnet)
test=data.table::fread("D:/Academics/613/Project/613data/HartestUncor1.csv", header=TRUE)
train=data.table::fread("D:/Academics/613/Project/613data/HartrainUncor1.csv", header=TRUE)
#test=read.csv('hartest.csv', header = TRUE, sep = ","  )
#train=read.csv('hartrain.csv', header = TRUE, sep = ","  )
#test=read.csv('hartestfit.csv', header = TRUE, sep = ","  )
x <- model.matrix(Activity ~ .,data=train)[, -1]
y=as.factor(train$Activity)
y
x
grid=10^seq(10,-2,length=100)

cvfit = cv.glmnet(x,y,type.measure="class",alpha=0,family="multinomial", lambda=grid, type.multinomial="grouped")
coef=coef(cvfit, s = "lambda.min")
coef
par(mar=c(1,1,1,1))
par(mfrow=c(6,1))
plot(coef$'0')
plot(coef$'1') 
plot(coef$'2')
plot(coef$'3')
plot(coef$'4')
plot(coef$'5')
     
plot(coef, c(0,1,2,3,4,5))