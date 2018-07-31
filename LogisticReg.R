SNA<- read.csv("Social_Network_Ads.csv")
SNA<- SNA[3:5]

set.seed(123)
library(caTools)

## Now split the data

split<-sample.split(SNA$Purchased, SplitRatio = 0.75)
train<- subset(SNA, split== T)
test<- subset(SNA, split == F)
# now we do the feature scaling
# to standardize the range of 
#independent variables

train[,1:2]<- scale(train[,1:2])
test[,1:2] <- scale(test[,1:2])

##Now we create a classifier for logistic Regression
classifier<- glm(formula = Purchased ~ .,
                 family = binomial,
                 data = train)
## now predict it with the test set
test_pred<- predict(classifier, 
                    newdata = test[-3],
                    type = "response")
y_pred<- ifelse(test_pred > 0.5,1,0)
## we make a Confusion Matrix to check for errors
cm<- table( test[,3], y_pred )
cm
##Visualising
install.packages("ElemStatLearn")
library(ElemStatLearn)

# here we are making the grid
set<- train
x1<- seq(min(set[1], -1), max(set[1], +1), 0.01 )
x2<- seq(min(set[2], -1), max(set[2], +1), 0.01)
g1<- expand.grid(x1,x2)


# since the grid represents a matrix, we build a matrix
colnames(g1)<- c("Age","EstimatedSalary")

grid_pred<- predict(classifier, newdata = g1, type = "response")
g_pred<- ifelse(grid_pred> 0.5,1,0)
plot(set[,-3], main= "Logistic Regression(training set) ", 
     xlab= "Age", ylab = "Estimated Salary",xlim=range(x1),ylim=range(x2) )
contour(x1,x2, add = T, matrix(as.numeric(g_pred), length(x1),length(x2)))
points(g1,pch = ".", col= ifelse(g_pred == 1,"springgreen3","tomato3"))
points(set, pch=21, bg= ifelse(set[,3]== 1,"green4","red3"))

##### now visualising the test set
set<- test
x1<- seq(min(set[1], -1), max(set[1], +1), 0.01 )
x2<- seq(min(set[2], -1), max(set[2], +1), 0.01)
g1<- expand.grid(x1,x2)


# since the grid represents a matrix, we build a matrix
colnames(g1)<- c("Age","EstimatedSalary")

grid_pred<- predict(classifier, newdata = g1, type = "response")
g_pred<- ifelse(grid_pred> 0.5,1,0)
plot(set[,-3], main= "Logistic Regression(test set) ", 
     xlab= "Age", ylab = "Estimated Salary",xlim=range(x1),ylim=range(x2) )
contour(x1,x2, add = T, matrix(as.numeric(g_pred), length(x1),length(x2)))
points(g1,pch = ".", col= ifelse(g_pred == 1,"springgreen3","tomato3"))
points(set, pch=21, bg= ifelse(set[,3]== 1,"green4","red3"))
