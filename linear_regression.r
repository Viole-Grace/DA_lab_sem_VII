linear <- read.csv("1.csv", header = TRUE, sep = ",")
x <- linear$experience
y <- linear$publications
m_x <-mean(x); m_y <- mean(y)

num = sum((x-m_x)*(y-m_y))
den = sum((x-m_x)^2)

b1 = num/den 
b0 = m_y- b1*m_x

linear$pred = b0 +b1*x

rss = sum((y-linear$pred)^2)
tss = sum((y-m_y)^2)

rse = sqrt(rss/nrow(linear)-2)
se = 1-(rss/tss)

fit <- lm(y~x, linear)
summary(fit)

linear$predicted = predict(fit, data.frame(x=c(x)))

plot(x,y)
lines(linear$pred)
lines(linear$predicted)