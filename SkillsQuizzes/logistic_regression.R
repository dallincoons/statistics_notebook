plot(gasbill ~ month, data = Utilities)
ulm <- lm(gasbill ~ month + I(month^2), data = Utilities)

b <- coef(ulm)
curve(b[1] + b[2]*x + b[3]*x^2, add = T)

plot(gasbill ~ month, data = Utilities)
uglm <- glm(gasbill > 80 ~ month + I(month^2), data=Utilities, family=binomial)

b <- uglm$coefficients

plot( formula = gasbill > 80 ~ month, data=Utilities, pch=16, cex=1.3)
curve(exp(b[1] + b[2]*x + b[3]*x^2)/(1+exp(b[1]+ b[2]*x + b[3]*x^2)), add=TRUE, col='skyblue', lwd=2)
