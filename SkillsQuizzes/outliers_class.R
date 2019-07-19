library(car)
library(MASS)

d.lm <- lm(weight ~ repwt, data=Davis)
r.lm <- rlm(weight ~ repwt, data=Davis)

b <- coef(d.lm)
rb <- coef(r.lm)

plot(weight ~ repwt, data=Davis, col="orange", pch=19)
curve(b[1] + b[2]*x, add=T, col="red", lwd=2)
curve(rb[1] + rb[2]*x, add=T, col="skyblue", lwd=2)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.1))
plot(d.lm, which=c(1,4))
plot(r.lm, which=c(1,4))

plot(d.lm, which=1:6)
