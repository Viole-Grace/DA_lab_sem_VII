mlr <- read.csv('2.csv', header = TRUE, sep = ',')
x1 <- mlr$experience; x2 <- mlr$training; y <- mlr$publications
mx1 <-mean(x1); mx2 <-mean(x2); my = mean(y)
n1 = sum((x1-mx1)*(y-my)); d1 = sum((x1-mx1)^2)
n2 = sum((x2-mx2)*(y-my)); d2 = sum((x2-mx2)^2)
b2 = n2/d2; b1 = n1/d1
b0 = my - b1*mx1 - b2*mx2

mlr$pred = b0+b1*x1+b2*x2

fit <- lm(y~x1+x2, data=mlr)
summary(fit)
mlr$lib = predict(fit, data.frame(x1=c(x1), x2=c(x2)))

plot(y, mlr$pred)
plot(mlr$pred, y, xlab="predicted", ylab="actual")
abline(a=0,b=1)

rss = sum((y-mlr$pred)^2)
tss = sum((y-my)^2)
rse = sqrt(rss/(nrow(mlr)-2))
se = 1-(rss/tss)