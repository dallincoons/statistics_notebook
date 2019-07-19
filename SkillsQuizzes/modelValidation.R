library(tidyverse)

dat <- read_csv('../Data/DallinsData.csv')
dat2 <- read_csv('../Data/DallinsData2.csv')

dat <- dat %>% mutate(myX =case_when((X8 < 0 & X8 > -13 & Y < -1) ~ 1,
                                       (X8 < -13 & Y > -1) ~ 1,
                                       (X8 > 0 & Y > -1) ~ 1),
                        myX = factor(ifelse(is.na(myX), 0, 1)))

lmt <- lm(Y ~ X5 + X2 + X5:X2 + I(X5^2) + I(X5^3) + I(X5^2):X2 + I(X5^3):X2, data = dat)
lm1 <- lm(Y ~ X8 + I(X8^2) +  myX:X8 + myX:I(X8^2), data = dat)
lm2 <- lm(Y ~ X5  + I(X5^2) + I(X5^3), data = dat)

summary(lmt)
summary(lm1)
summary(lm2)

plot(Y ~ X5, data = dat2)

b <- coef(lmt)

curve(b[1] +b[2]*x + b[4]*x^2 + b[5]*x^3, add=TRUE, col=palette()[1], lwd=4)
curve((b[1] + b[3]) + (b[2] + b[6])*x + (b[4] + b[7])*x^2 + (b[5] + b[8])*x^3, add=TRUE, col=palette()[1], lwd=4)

validate <- function(lm, data) {
  yh1 <- predict(lm, newdata=data)
  
  ybar <- mean(data$Y)
  
  SSTO <- sum( (data$Y - ybar)^2 )
  
  SSE <- sum( (data$Y - yh1)^2 )
  
  rs1 <- 1 - SSE/SSTO
  
  n <- length(data$Y)
  p <- length(coef(lmt))
  
  rsa <- 1 - (n-1)/(n-p)*SSE/SSTO
  
  return(rsa)
}

print(validate(lmt, dat2))

print(validate(lm1, dat2))

print(validate(lm2, dat2))


