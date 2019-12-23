qda <-read.csv('8.csv')
summary(qda)
qda
library(MASS)
library(ggplot2)
dim(qda)

set.seed(1)
row.number = sample(1:nrow(qda), 0.7*nrow(qda))
train = qda[row.number,]
test = qda[-row.number,]

attach(train)
mod <- qda(default~student+balance+income, data=train)
summary(mod)

