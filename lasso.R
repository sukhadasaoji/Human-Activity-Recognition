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

cvfit = cv.glmnet(x,y,type.measure="class",alpha=1,family="multinomial", lambda=grid, type.multinomial="grouped")
coef=coef(cvfit, s = "lambda.min")
coef
par(mar=c(1,1,1,1))
par(mfrow=c(6,1))
plot(coef$'0', xlabel="Laying", ylabel= "Predictors")
plot(coef$'1', xlabel="Sitting,, ylabel="Predictors") 
plot(coef$'2', xlabel= "=Standing",  ylabel= "Predictors")
plot(coef$'3', xlabel= "Walking",  ylabel= "Predictors")
plot(coef$'4', xlabel="Walking_Downstairs", ylabel= "Predictors")
plot(coef$'5', xlabel=" Walking_Upstairs", ylabel="Predictors")
plot(coef, c(0,1,2,3,4,5))

coef[is.na(coef)]<-0
coef
unlist(coef)

#write.table(coef,"test.txt",sep=";")
class(coef)
coef_df <- data.frame(matrix(unlist(coef)))
print(coef_df)

data.frame(predictor      = rownames(coef)[sum$Length],
           Destination = colnames(coef)[sum$Class],
           Weight      = sum$Mode)
coef_m <- as.data.frame(as.matrix(coef)) 
coef_m
coef_df = as.list.data.frame(as.list(coef))
coef_df=cbind(as.list.data.frame(coef))
coef_df
coef_1=as.data.frame(as.numeric(coef_df))
lasso_out = rownames(coef_df)[which(coef_df[,1] != ".", )][-1]
lasso_out
imp_coef=complete.cases(coef)
lasso_out
#glmmod <- glmnet(x, y, alpha=1, lambda=grid, family="multinomial")
#summary(glmmod)

#plot(glmmod)
