#import library for counfusion matrix
library(caret)

#import dataset
data1 <- read.csv("logistic/day.csv")

#discrad not needed columns date and index
data1=data1[,-1]
data1=data1[,-1]

#look the data
head(data1)


#convert value of cnt which is target varible into 1 and 0 . here look at the summary and pick up mean of cnt coloum for define 1 and 0.
i=1
while(i<732){
  if(data1[i,14]>=4504){
    data1[i,14]=1
  }
  else{
    data1[i,14]=0
  }
  i=i+1
}

#counting total numbers of 0 and 1
table(data1[,14])

#define logistic model and give require parameters like formula target~independent variable , family= binomial shows logistic regression
log_bike = glm(formula = cnt~. ,data = data1 , family = binomial)

#summary of model
summary(log_bike)

#predict values and ploat it and summarization
a=predict(log_bike)
plot(a)
summary(a)

#using tappply for find the class
tapply(a,data1[,14] , mean)

#convert predicted values into 1 and 0 .using summary of a to define boundry 
i=1
while(i<732){
  if(a[i]>=18.93){
    a[i]=1
  }
  else{
    a[i]=0
  }
  i=i+1
}

#using cunsusion matrix for accuracy
confusionMatrix(table(a,data1[,14]),positive='1')
