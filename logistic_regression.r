library(MASS)
library(ggplot2)

log <-read.csv('6.csv', header=TRUE, sep=',')
log
dim(log)  
str(log)
summary(log)

attach(log)
set.seed(1)
row.number = sample(1:nrow(log), 0.6*nrow(log))
train = log[row.number,]; test=log[-row.number,]

attach(train)
model <- glm(default~student+balance+income, data=train, family = binomial)
summary(model)
model

pred.prob = predict(model, type='response')
pred.prob = ifelse(pred.prob>0.5,1,0)
table(pred.prob, train$default)

attach(test)
pred.prob = predict(model, newdata = test, type = 'response')
pred.prob = ifelse(pred.prob<0.5,0,1)
table(pred.prob, test$default)