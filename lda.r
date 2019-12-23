lda <- read.csv('7.csv')
lda
summary(lda)
dim(lda)

library(MASS)
library(ggplot2)
set.seed(1)

row.number = sample(1:nrow(lda), 0.7*nrow(lda))
train = lda[row.number,]
test = lda[-row.number,]

attach(train)
model <- lda(default~student+balance+income, data=train)
summary(model)

pred = predict(model, data=train, type='response')
table(pred$class, default)

attach(test)
pred2 = predict(model, newdata=test, type='response')
table(pred2$class, default)

ldahist(pred$x[,1], g=pred$class)
par(mfrow=c(1,1))
plot(pred2$x[,1], pred2$class, col=test$default)