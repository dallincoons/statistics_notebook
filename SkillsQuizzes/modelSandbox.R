set.seed(151)

n <- 3000

X1 <- runif(n, -5, 5)
X2 <- sample(c(0, 1), n, replace=TRUE)

beta0 <- -2
beta1 <- 0
beta2 <- -.8
beta3 <- .8

beta4 <- 4
beta5 <- -4.1
beta6 <- 3.225
beta7 <- -0.8

sigma <- 20

Y <- beta0 + beta1*X1 + beta2*X1^2 + beta3*X1^3 +
     beta4*X2 + beta5*X1*X2 + beta6*X1^2*X2 + beta7*X1^3*X2 +
  rnorm(n, 0, sigma)

myData <- data.frame(
            Y = Y
            ,
            X1 = X1
            ,
            X2 = X2
            ,
            X3 = runif(n, 20,40)
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

summary(lm(Y ~ X1 + X2 + I(X1^2) + I(X1^3) + X1:X2 + I(X1^2):X2, data = myData))
