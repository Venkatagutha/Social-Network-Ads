SNA<-read.csv("Social_Network_Ads.csv")
SNA<-SNA[,3:5]

SNA$Purchased<-factor(SNA$Purchased, levels = c(0,1))

#split
library(caTools)
set.seed(123)
split<- sample.split(SNA$Purchased, SplitRatio = 0.75)
train<- subset(SNA,split==T)
test<- subset(SNA,split==F)
## feature scaling
train[,-3]<-scale(train[,-3])
test[,-3]<-scale(test[,-3])
##classifier
library(rpart)
dt<- rpart(formula = Purchased ~ ., data = train)
y_pred<-predict( dt, newdata= test, type = "class")
##build the confusion matrix
cm<- table(test[,3],y_pred)
##Visualisation

library(ElemStatLearn)
set<-train
x1<- seq(min(set[,1],-1), max(set[,1],+1),0.01)
x2<- seq(min(set[,2],-1), max(set[,2],+1),0.01)
g1<-expand.grid(x1,x2)
colnames(g1)<-c("Age","EstimatedSalary")
g_pred<- predict(dt, newdata = g1, type="class")
plot(set[-3], xlim=range(x1),ylim=range(x2),
     main="DecisionTree (training set)", xlab="Age",ylab="ExpectedSalary")
contour(x1,x2, matrix(as.numeric(g_pred),length(x1),length(x2)),add=T)
points(g1,pch=".",col=ifelse(g_pred==1,"springgreen3","tomato3"))
points(set[,-3], pch=21, bg=ifelse(set[,3]==1,"green3","red4"))

##Test

library(ElemStatLearn)
set<-test
x1<- seq(min(set[,1],-1), max(set[,1],+1),0.01)
x2<- seq(min(set[,2],-1), max(set[,2],+1),0.01)
g1<-expand.grid(x1,x2)
colnames(g1)<-c("Age","EstimatedSalary")
g_pred<- predict(dt, newdata = g1, type="class")
plot(set[-3], xlim=range(x1),ylim=range(x2),
     main="DecisionTree (test set)", xlab="Age",ylab="ExpectedSalary")
contour(x1,x2, matrix(as.numeric(g_pred),length(x1),length(x2)),add=T)
points(g1,pch=".",col=ifelse(g_pred==1,"springgreen3","tomato3"))
points(set[,-3], pch=22, bg=ifelse(set[,3]==1,"green3","red4"))

###Viewing the decision tree
plot(dt)
text(dt)
?text()



