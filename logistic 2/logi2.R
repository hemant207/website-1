library (caret)
library(class)

data1=read.csv("knn/cancer.csv")
View(data1)

train=data1[1:70,-1]
test=data1[71:100,-1]


summary(train)

head(train)
str(test)

pr_c <- glm(formula = diagnosis_result~. ,data = train , family = binomial)
pr_c

summary(pr_c)

a=predict(pr_c,type="response")

summary(a)

tapply(a,train[1],mean)
plot(a)

i=1

while(i<=70){
  if(a[i]<=0.7592025){
    a[i]="B"
  }else{
    a[i]="M"
  }
  i=i+1
}
head(a) 

table(a,train[1])
head(train[1])

confusionMatrix(table(a,train[1:70,1]),positive ='M')

