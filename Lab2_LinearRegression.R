library(ISLR2)
library(MASS)


par(mfrow = c(1, 1))
cat.lm = lm(Hwt ~ Bwt, data=cats)
plot(cats$Bwt, cats$Hwt,
     pch = 20,
     col = "steelblue",
     xlab = "Body Weight [kg]",
     ylab = "Heart Weight [g]",
     main = "Heart Weight vs Body Weight for Cats")
abline(cat.lm, lwd = 3, col = "red")

summary(cat.lm)
par(mfrow = c(2, 2))
plot(cat.lm, pch = 20, col = "steelblue")


usc.lm = lm(y ~ M + Ed + Po1 + U2 + Ineq, data=UScrime)
summary(usc.lm)
par(mfrow = c(2, 2))
plot(usc.lm, pch = 20, col = "steelblue")