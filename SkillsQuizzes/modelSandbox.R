set.seed(151)

n <- 3000

X5 <- runif(n, -10, 10)
X2 <- sample(c('blue pill', 'red pill', 'mystery pill', 'green pill'), n, replace=TRUE)

blue <- ifelse(X2 == 'blue pill', 1, 0)
red <- ifelse(X2 == 'red pill', 1, 0)
green <- ifelse(X2 == 'green pill', 1, 0)
mystery <- ifelse(X2 == 'mystery pill', 1, 0)

beta0 <- 4
beta1 <- .2
beta2 <- .03
beta3 <- -.01

beta4 <- 1
beta5 <- -.5
beta6 <- .02
beta7 <- .02

beta8 <- 5
beta9 <- -.09
beta10 <- -.18
beta11 <- .009

beta12 <- -2
beta13 <- -.9
beta14 <- -.02
beta15 <- 0

beta16 <- -10
beta17 <- -.1
beta18 <- .1
beta19 <- 0

sigma <- 5

Y <- 
     beta0 + beta1*X5 + beta2*X5^2 + beta3*X5^3 +
     beta4*blue + beta5*X5*blue + beta6*X5^2*blue + beta7*X5^3*blue +
     beta8*red + beta9*X5*red + beta10*X5^2*red + beta11*X5^3*red +
     beta12*green + beta13*X5*green + beta14*X5^2*green + beta15*X5^3*green +
     beta16*mystery + beta17*X5*mystery + beta18*X5^2*mystery + beta19*X5^3*mystery +
  rnorm(n, 0, sigma)

myData <- data.frame(
            Y = Y
            ,
            X1 = rf(n, 3, 9)
            ,
            X2 = X2
            ,
            X3 = sample(c(0,1), n, replace=TRUE)
            ,
            X4 = rf(n, 2, 5)
            ,
            X5 = X5
            ,
            X6 = rf(n, 2, 5) + rt(n, 1)^2
            ,
            X7 = sample(c(0,1), n, replace=TRUE)
            ,
            X8 = runif(n, -20, 5)
            ,
            X9 = rpois(n, 3.2)
            ,
            X10 = rbinom(n, 30, .5)
          )

plot(Y ~ X5, data = myData)


# pairs(myData)

mylm <- lm(Y ~ X5 + X2 + X5:X2 + I(X5^2) + I(X5^3) + I(X5^2):X2 + I(X5^3):X2, data = myData)

summary(mylm)

b <- coef(mylm)

curve(b[1] + b[2]*x + b[6]*x^2 + b[7]*x^3, add=TRUE, col=palette()[1], lwd=4)
curve((b[1] + b[3]) + (b[2] + b[8])*x + (b[6] + b[11])*x^2 + (b[7] + b[14])*x^3, add=TRUE, col=palette()[1], lwd=4)
curve((b[1] + b[4]) + (b[2] + b[9])*x + (b[6] + b[12])*x^2 + (b[7] + b[15])*x^3, add=TRUE, col=palette()[1], lwd=4)
curve((b[1] + b[4]) + (b[2] + b[9])*x + (b[6] + b[12])*x^2 + (b[7] + b[15])*x^3, add=TRUE, col=palette()[1], lwd=4)
curve((b[1] + b[5]) + (b[2] + b[10])*x + (b[6] + b[13])*x^2 + (b[7] + b[16])*x^3, add=TRUE, col=palette()[1], lwd=4)

# lm1 <- lm(Y ~ X4, data = myData)
# 
# lm(formula = Y ~ X1, data = myData)
# 
# palette(c("skyblue","orange"))
# pairs(cbind(R=lm1$res, fit=lm1$fit, myData), pch=16, cex=1, panel=panel.smooth, col.smooth="skyblue4", col=factor(myData$X1))
