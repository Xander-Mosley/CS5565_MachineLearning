library(ISLR2)
library(boot)



attach(Auto)
set.seed(0464)
train <- sample(392, 196)

lm.fit <- lm(mpg ~ horsepower, data = Auto,
             subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

## [1] 23.26601
## [1] 18.71646
## [1] 18.79401

## [1] 25.72651
## [1] 20.43036
## [1] 20.38533



set.seed(0464)
train <- sample(392, 196)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

train <- sample(392, 294)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

train <- sample(392, 343)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto,
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

train <- sample(392, 319)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto,
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

train <- sample(392, 306)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto,
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)



set.seed(0464)
cv.error <- rep(0, 8)
for (i in 1:8) {
  glm.fit <- glm(mpg ~ poly(weight, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error



set.seed(0464)
cv.error.5 <- rep(0, 5)
for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(displacement, i), data = Auto)
  cv.error.5[i] <- cv.glm(Auto, glm.fit, K = 5)$delta[1]
}
cv.error.5

cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(displacement, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10



set.seed(0464)
boot.fn <- function(data, index)
  coef(
    lm(mpg ~ horsepower + I(horsepower^2), 
       data = data, subset = index)
  )
boot(Auto, boot.fn, 1000)

boot(Auto, boot.fn, 250)
boot(Auto, boot.fn, 500)
boot(Auto, boot.fn, 2500)