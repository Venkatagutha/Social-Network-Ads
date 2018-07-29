SNA<- read.csv("Social_Network_Ads.csv")
SNA<-SNA[,3:5]
SNA$Purchased<- factor(SNA$Purchased, levels = c(0,1))# it is a categorical variable

##Split the data
set.seed(123)
split<- sample.split(SNA$Purchased, SplitRatio = 0.75)
train<- subset(SNA, split==T)
test<- subset(SNA, split==F)
##Scale
train[-3]<- scale(train[-3])
test[-3]<- scale(test[-3])
## build the classifier
install.packages("randomForest")
library(randomForest)
rf<- randomForest(x= train[-3], y=train$Purchased, ntree = 10)
rf_pred<- predict(rf, newdata = test[-3])
## build the confusion matrix
cm<- table(test[,3], rf_pred)
## visualise
library(ElemStatLearn)
set<- train
x1<- seq(min(set[1],-1),max(set[1], +1),0.01)
x2<- seq(min(set[2],-1),max(set[2], +1),0.01)
g1<- expand.grid(x1,x2)
colnames(g1)<-c("Age","EstimatedSalary")
g1_pred<- predict(rf, newdata = g1)
plot(set[,-3], main="Random Forest(Training Set)", xlab="Age", ylab="ExpectedSalary",
     xlim=range(x1), ylim=range(x2))
contour(x1,x2,matrix(as.numeric(g1_pred),length(x1),length(x2)), add=T)
points(g1, pch=".", col=ifelse(g1_pred==1,"springgreen3","tomato3"))
points(set[,-3],pch=21, bg= ifelse(set[,3]==1,"green3","red4"))
##Test set##
set<- test
x1<- seq(min(set[1],-1),max(set[1], +1),0.01)
x2<- seq(min(set[2],-1),max(set[2], +1),0.01)
g1<- expand.grid(x1,x2)
colnames(g1)<-c("Age","EstimatedSalary")
g1_pred<- predict(rf, newdata = g1)
plot(set[,-3], main="Random Forest(Test Set)", xlab="Age", ylab="ExpectedSalary",
     xlim=range(x1), ylim=range(x2))
contour(x1,x2,matrix(as.numeric(g1_pred),length(x1),length(x2)), add=T)
points(g1, pch=".", col=ifelse(g1_pred==1,"springgreen3","tomato3"))
points(set[,-3],pch=21, bg= ifelse(set[,3]==1,"green3","red4"))
###
