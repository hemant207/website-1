library(caret)

binary <- read.csv("binary.csv")
b=binary[,-2]
head(b)

str(binary)
summary(binary[1])

summary(binary)

mod <- glm(formula = admit~. ,data=b , family = binomial )
mod
summary(mod)
plot(mod)

a = predict(mod,type="response")

tapply(a,b$admit,mean)
summary(a)
plot(a)

confusionMatrix(table(a,b[,1]),positive = "0")
table(a)
table(b[,1])
i=1


while(i<=400){
if (a[i]<=0.31750){
  a[i]=0
}else{
  a[i]=1
}
i=i+1
}
