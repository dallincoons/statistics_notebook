X <- c(1, 4,  7,   8,  10, 20)
Y <- c(3, 5, 18, 13, 12,   1)
w <- c(0, 0, 1, 0, 0, 1)
w <- c(1, 1, .5, .8, 1, .2)
w <- c(.25, .25, 1, 1, .8, .8)

mylm <- lm(Y ~ X, weights=w)

plot(Y ~ X, pch=21, bg=rgb(1-w,1-w,1-w), col="orange")

abline(mylm)

u.lm <- lm(gasbill ~ month + I(month^2), data = Utilities)
b <- coef(u.lm)


plot(gasbill ~ month, data = Utilities, col="orange",pch=19)
curve(b[1] + b[2]*x + b[3]*x^2, add = T, col="skyblue", lwd=2)
lines(lowess(Utilities$month, Utilities$gasbill), col="firebrick", lwd=2)

ggplot(Utilities, aes(month, gasbill)) +
  geom_point() +
  geom_smooth()
