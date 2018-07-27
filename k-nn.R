SNA<- read.csv("Social_Network_Ads.csv")
SNA<- SNA[,3:5]
library(caTools)
library(ggplot2)
set.seed(123)
## NOW SPLIT THE DATA INTO TRAINING AND TEST SET

split<- sample.split(SNA$Purchased, SplitRatio = 0.75)
train<- subset(SNA, split==T)
test<- subset(SNA, split== F) 
## Feature Scaling
train[,1:2]<- scale(train[,1:2])
test[,1:2]<- scale(test[,1:2])
## build the classifier
install.packages("class")
library(class)

v_pred<- knn( train = train[,-3], test = test[,-3], cl = train[,3], k=5 )
#bulid a confusion matrix to check
cm<- table(test[,3],v_pred)

### Visualising Training
library(ElemStatLearn)
set<- train
x1<- seq(min(set[,1], -1), max(set[,1], +1), 0.01)
x2<- seq(min(set[,2], -1), max(set[,2], +1), 0.01)
g1<- expand.grid(x1,x2)
colnames(g1)<- c("Age","ExpectedSalary")
g1_pred <- knn(train = train[,-3], test = g1, cl = train[,3], k=5 )
plot(set[,-3], xlim = range(x1),ylim = range(x2),
        xlab="Expected Salary", ylab="Age", main="K-NN (Training Set)")
contour(x1,x2,matrix(as.numeric(g1_pred), length(x1),length(x2)), add = T)
points(g1,pch="." ,col = ifelse(g1_pred==1,"springgreen3","tomato3"))
points(set, pch=21, bg = ifelse(set[,3]==1,"green4","red4"))

##Test set
set1<- test
y1<- seq(min(set1[1], -1), max(set[1], +1), 0.01)
y2<- seq(min(set[2], -1), max(set[2], +1), 0.01)
g2<- expand.grid(y1,y2)

g2_pred<- knn(train = train[,-3], test = g2, cl= train[,3] , k=5)
plot(set1[,3], main = "K-NN(Test Set)", xlab="Expected Salary", ylab="Age",
     xlim = range(y1), ylim = range(y2))
contour(y1,y2, add= T, matrix(as.numeric(g2_pred),length(y1),length(y2)))
points(g2, pch=".",col=ifelse(g2_pred==1,"springgreen","Tomato3"))
points(set1, pch= 21, bg= ifelse(set1[,3]==1,"green3","red4"))
