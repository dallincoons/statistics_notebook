set.seed(151)

n <- 3000

X1 <- runif(n, -10, 10)
X2 <- sample(c('blue pill', 'red pill', 'mystery pill'), n, replace=TRUE)

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

beta8 <- 1.5
beta9 <- -.09
beta10 <- -.18
beta11 <- .009

beta12 <- -2
beta13 <- -.19
beta14 <- -.02
beta15 <- 0

beta16 <- -3
beta17 <- -.6
beta18 <- -.06
beta19 <- .0

sigma <- 1

Y <- 
     beta0 + beta1*X1 + beta2*X1^2 + beta3*X1^3 +
     beta4*blue + beta5*X1*blue + beta6*X1^2*blue + beta7*X1^3*blue +
     beta8*red + beta9*X1*red + beta10*X1^2*red + beta11*X1^3*red +
     # beta12*green + beta13*X1*green + beta14*X1^2*green + beta15*X1^3*green +
     beta16*mystery + beta17*X1*mystery + beta18*X1^2*mystery + beta19*X1^3*mystery +
  rnorm(n, 0, sigma)

myData <- data.frame(
            Y = Y
            ,
            X1 = X1
            ,
            X2 = X2
            ,
            X3 = rf(n, 3, 9)
            ,
            X4 = rf(n, 2, 5)
            ,
            X5 = sample(c(0,1), n, replace=TRUE)
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

plot(Y ~ X1, data = myData)


# pairs(myData)

mylm <- lm(Y ~ X1 + X2 + X1:X2 + I(X1^2) + I(X1^3) + I(X1^2):X2 + I(X1^3):X2, data = myData)

summary(mylm)

b <- coef(mylm)

# curve(b[1] + b[2]*x + b[3]*x^2 + b[4]*x^3, add=TRUE, col=palette()[1], lwd=4)
