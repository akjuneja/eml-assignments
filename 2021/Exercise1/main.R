# Assignment 1, Problem 6

# Problem 6.1
#install.packages("ISLR2")
#install.packages("glmnet")
library(ISLR2)
library(glmnet)
plot(Auto)

# Problem 6.2
cor(Auto$mpg, Auto$weight)
cor(Auto$weight, Auto$displacement)

# Problem 6.3
lm1.fit <- lm(mpg ~ cylinders, data=Auto)
summary(lm1.fit)

lm2.fit <- lm(mpg ~ displacement, data=Auto)
summary(lm2.fit)

lm3.fit <- lm(mpg ~ horsepower, data=Auto)
summary(lm3.fit)

lm4.fit <- lm(mpg ~ year, data=Auto)
summary(lm4.fit)

# Problem 6.4
lm.fit <- lm(mpg ~ . - name, data=Auto)
summary(lm.fit)

# Problem 6.5
par (mfrow = c(2, 2))
plot(lm.fit)