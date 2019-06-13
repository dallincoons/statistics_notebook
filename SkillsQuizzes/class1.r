plot(gasbill ~ temp, data = Utilities)
lm1 <- lm(gasbill ~ temp, data = Utilities)

abline(lm1, col="hotpink")

plot(lm1, which = 1)

boxCox(lm1)
boxCox(lm1, lambda = seq(0, .5, .1))

plot(sqrt(sqrt(gasbill)) ~ temp, data = Utilities)
abline(lmT)

plot(gasbill ~ temp, data = Utilities)

lmT <- lm(sqrt(sqrt(gasbill)) ~ temp, data = Utilities)

b <- coef(lmT)

abline(lm1, col="hotpink")
curve((b[1] + b[2] * x) ^ 4, add=TRUE, col="skyblue", lwd=2)

predict(lmT, data.frame(temp=30), interval="prediction")

abline(h=(predict(lmT, data.frame(temp=30), interval="prediction"))^4, lty=2, lwd = 2, col="skyblue")

abline(h=predict(lm1, data.frame(temp=30), interval="prediction"), lty=2, lwd = 2, col="hotpink")

abline(v = 30, lty=2, col="skyblue")
