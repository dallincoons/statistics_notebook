set.seed(151)

n <- 80

X5 <- runif(n, -10, 10)
X2 <- sample(c('blue pill', 'red pill'), n, replace=TRUE)
X9 <- sample(c(0, 1), n, replace=TRUE)

X3 <- sample(c('one fish', 'two fish', 'red fish', 'blue fish'), n, replace=TRUE)

X6 <- sample(c('steak', 'chicken', 'mystery meat'), n, replace=TRUE)

X8 <- runif(n, -10, 10)

red <- ifelse(X2 == 'red pill', 1, 0)

beta0 <- -5
beta1 <- .6
beta2 <- .03
beta3 <- -.01

beta4 <- 3
beta5 <- -1.2
beta6 <- -.06
beta7 <- .02

beta8 <- 1
beta9 <- .1
beta10 <- 0
beta11 <- 0

beta12 <- 0
beta13 <- -.3
beta14 <- 0
beta15 <- 0

sigma <- 2.9

Y <- 
     beta0 + beta1*X5 + beta2*X5^2 + beta3*X5^3 +
     beta4*red + beta5*X5*red + beta6*X5^2*red + beta7*X5^3*red +
     beta8 + beta9*X5 +
     beta12*X9 + beta13*X9*X5 +
  rnorm(n, 0, sigma)

myData <- data.frame(
            Y = Y
            ,
            X1 = rbeta(n,5,2)
            ,
            X2 = X2
            ,
            X3 = X3
            ,
            X4 = rf(n, 2, 5)
            ,
            X5 = X5
            ,
            X6 = X6
            ,
            X7 = sample(c('right','left'), n, replace=TRUE)
            ,
            X8 = runif(n, -20, 5)
            ,
            X9 = X9
            ,
            X10 = rbinom(n, 30, .5)
          )

plot(Y ~ X5, data = myData)

# pairs(myData)

mylm <- lm(Y ~ X5 + X2 + X5:X2 + X5:X9 + I(X5^2) + I(X5^3) + I(X5^2):X2 + I(X5^3):X2, data = myData)

summary(mylm)

b <- coef(mylm)

curve(beta0 + beta1*x + beta2*x^2 + beta3*x^3, add=TRUE, col=palette()[1], lwd=4)
curve((beta0 + beta4) + (beta1 + beta5)*x + (beta2 + beta6)*x^2 + (beta3 + beta7)*x^3, add=TRUE, col=palette()[1], lwd=4)
abline(beta8, beta9)
abline((beta8 + beta12), (beta9 + beta13))

# lm1 <- lm(Y ~ X4, data = myData)
# 
# lm(formula = Y ~ X1, data = myData)
# 
# palette(c("skyblue","orange"))
# pairs(cbind(R=lm1$res, fit=lm1$fit, myData), pch=16, cex=1, panel=panel.smooth, col.smooth="skyblue4", col=factor(myData$X1))
