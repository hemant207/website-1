#import libraries for confusion matrix and knn algorithm

library(caret)
library(class)

#import cancer data set
data <- read.csv("knn/cancer.csv")

#splite dataset into train data and test data
train=data[1:80,]
test = data[81:100,]

#see the datatype of available variables in the dataset
str(train)
str(test)

#review the data which will be used in model 
summary(train)
summary(test)

#build knn classification model
md=knn(train=train[,-2],test = test[,-2],cl=train$diagnosis_result,k=3)

#see the output of model
md

#create confusion matrix and check the Accuracy of model
confusionMatrix(table(md,test$diagnosis_result))
