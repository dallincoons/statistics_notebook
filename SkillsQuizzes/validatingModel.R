set.seed(123) #gives us the same randomness 
n <- 20
X <- runif(n, -1.5, 3.8)

beta0 <- 2
beta1 <- -2.5
beta2 <- 1
beta3 <- 3
beta4 <- -0.8

Y <- beta0 + beta1*X + beta2*X^2 + beta3*X^3 + beta4*X^4 + rnorm(n, 0, 0.5)

par(mai=c(.1,.5,.2,.1))
plot(Y ~ X, pch=21, col="lightgray", bg="steelblue", cex=1.3, ylim=c(-5,22), yaxt='n', xaxt='n', ylab="", xlab="")

curve(beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, add=TRUE, col=rgb(0.2745098, 0.5098039, 0.7058824, .5), lwd=4)

lmt <- lm(Y ~ X + I(X^2) + I(X^3) + I(X^4))

lms <- lm(Y ~ X)

b <- coef(lms)
curve(b[1] + b[2]*x, add=TRUE, col=rgb(1,0.6470588,0, .3), lwd=2)

lmo <- lm(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10) + I(X^11) + I(X^12) + I(X^13) + I(X^14))
b <- coef(lmo)
curve(b[1] + b[2]*x + b[3]*x^2 + b[4]*x^3 + b[5]*x^4 + b[6]*x^5 + b[7]*x^6 + b[8]*x^7 + b[9]*x^8 + b[10]*x^9 + b[11]*x^10 + b[12]*x^11 + b[13]*x^12 + b[14]*x^13 + b[15]*x^14, add=TRUE, col=rgb(0.6980392, 0.133333, 0.133333, .2), lwd=2)

legend("topleft", legend=c("True Model", "Simple Model", "Complicated Model"), lwd=c(4,2,2), col=c(rgb(0.2745098, 0.5098039, 0.7058824, .5), rgb(1,0.6470588,0, .3), rgb(0.6980392, 0.133333, 0.133333, .2)), bty='n')

yht <- predict(lmt, newdata=data.frame(X=Xnew))
yhs <- predict(lms, newdata=data.frame(X=Xnew))
yhc <- predict(lmc, newdata=data.frame(X=Xnew))

ybar <- mean(Ynew)
