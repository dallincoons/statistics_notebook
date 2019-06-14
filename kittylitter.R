##

set.seed(151)

n <- 3000

X1 <- runif(n, 0, 3)
X3 <- runif(n, 0, 3)
X2 <- sample(c(0,1), n, replace=TRUE)

beta0 <- 2
beta1 <- 1

beta2 <- 1
beta3 <- 1.1

sigma <- 2.5

Y <- beta0 + beta1*X1 +
     beta2 + beta3 +
  rnorm(n, 0, sigma)

myData <- data.frame(Y = Y
                     , 
                     X1 = X1
                     , 
                     X2 = X2
                     ,
                     X3 = X3
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

# pairs(myData)

mylm <- lm(Y ~ X1, data=myData)
summary(mylm)

plot(Y ~ X1, data=myData)
b <- coef(mylm)
curve(b[1] + b[2]*x, add=TRUE, col=palette()[1], lwd=4)
