##

set.seed(151)

n <- 3000

X1 <- runif(n, 0, 3) 
X2 <- sample(c(0,1), n, replace=TRUE)

beta0 <- 2
beta1 <- 1.5
beta2 <- -0.125
beta3 <- 0

beta4 <- 18
beta5 <- 4.1
beta6 <- 3.225
beta7 <- -0.8

sigma <- 2.5

Y <- beta0 + beta1*X1 + beta2*X1^2 + beta3*X1^3 + 
  beta4*X2 + beta5*X1*X2 + beta6*X1^2*X2 + beta7*X1^3*X2 +
  rnorm(n, 0, sigma)
  
myData <- data.frame(Y = Y
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

pairs(myData)
  
mylm <- lm(Y ~ X1 + I(X1^2) + I(X1^3) + X2 + X1:X2 + I(X1^2):X2 + I(X1^3):X2, data=myData)
summary(mylm)

plot(Y ~ X1, data=myData)
b <- coef(mylm)
curve(b[1] + b[2]*x + b[3]*x^2 + b[4]*x^3, add=TRUE, col=palette()[1], lwd=4)
curve(b[1] + b[2]*x + b[3]*x^3 + b[4]*x^3 + b[5]*1 + b[6]*x*1 + b[7]*x^2*1 + b[8]*x^3*1, add=TRUE, col=palette()[2], lwd=4)
