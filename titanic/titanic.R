#import libraries
library(caret)
library(ggplot2)
library(dplyr)

#import train and test datasets of titanic
train <- read.csv(file = "titanic/train.csv")
test <- read.csv(file = "titanic/test.csv")

#have a look on data
head(train)
str(train)

#finding n/a values and replace them with the mean value
mean(test$Age,na.rm = TRUE)

train$Age[is.na(train$Age)]<- 30
test$Age[is.na(test$Age)]<- 30

test$Fare[is.na(test$Fare)]<- 35

#delete unnecerry columns
train = train[,c(-1,-4)]
test = test[,c(-1,-3)]


summary(train)
summary(test)

 
#change the values type as numeric for catogarical values
train$Sex<- as.numeric(train$Sex)
train$Ticket <- as.numeric(train$Ticket)
train$Cabin <- as.numeric(train$Cabin)
train$Embarked <- as.numeric(train$Embarked)

test$Sex<- as.numeric(test$Sex)
test$Ticket <- as.numeric(test$Ticket)
test$Cabin <- as.numeric(test$Cabin)
test$Embarked <- as.numeric(test$Embarked)

#find the corelation between variables
cor(train)
plot(train)

train = train[,-7]
test = test[,-6]

#build model 
model = glm(Survived~.,data=train,family ="binomial")
summary(model)

#make prediction
a<- predict(model)

#find the mean of predited values and according to that make condition 
tapply(a,train$Survived, mean)

i=1
plot(model$fitted.values,rstandard(model))

while(i<=891) {
  if(a[i]<=-0.7342){
    a[i]=0
  }else{
    a[i]=1
  }
  i=i+1
}

summary(a)

#find the accuracy
confusionMatrix(table(a,train$Survived))

#make prediction for the test data set
b<- predict(model,test)
summary(b)
i=1
while(i<=418) {
  if(b[i]<=-0.458){
    b[i]=0
  }else{
    b[i]=1
  }
  i=i+1
}

