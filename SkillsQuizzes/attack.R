library(tidyverse)

# dat <- read_csv('./RBdata.csv')
# dat$X8 <- as.factor(dat$X8)
# 
# test.lm <- lm(Y ~ X5 + I(X5^2) + I(X5^3) + X6 + X6:X5 + X6:I(X5^2), data = dat)
# 
# summary(test.lm)
# 
# b <- coef(test.lm)
# 
# pairs(cbind(R = test.lm$residuals, Fit = test.lm$fit, dat))
# 
# pairs(cbind(R=test.lm$res, fit=test.lm$fit, dat), pch=16, cex=1, panel=panel.smooth, col.smooth="skyblue4", col=factor(dat$X6))
# 
# plot(Y ~ X5, data=dat, col=factor(X6))
# curve(b[1] + b[2]*x + b[3]*x^2 + b[4]*x^3, add =TRUE)
# curve((b[1] + b[5]) + (b[2] + b[6])*x + (b[3] + b[7])*x^2 + b[4]*x^3, add =TRUE, col=palette()[2])


# dat <- read_csv('./RBdata2.csv')
# 
# pairs(dat)
# 
# test.lm <- lm(Y ~ X9 + X1 + X2 + X7 + X4 + X5 + X10 + X3:X6 + X8:X10, data = dat)
# 
# summary(test.lm)
# 
# pairs(cbind(R = test.lm$residuals, Fit = test.lm$fit, dat))
# 
# pairs(cbind(R=test.lm$res, fit=test.lm$fit, dat), pch=16, cex=1, panel=panel.smooth, col.smooth="skyblue4", col=factor(dat$X8))

# pairs(cbind(R=test.lm$res, fit=test.lm$fit, dat), pch=16, cex=1, panel=panel.smooth, col.smooth="skyblue4", col=factor(dat$X4))

dat <- read_csv('./battleship3.csv')

pairs(dat)

dat <- dat %>% 
  mutate(X1 = X1_1)

test.lm <- lm(Y ~ X1 + X1:X9 + X1:X7:X9, data = dat)

summary(test.lm)

b <- coef(test.lm)

pairs(cbind(R = test.lm$residuals, Fit = test.lm$fit, dat))

# pairs(cbind(R=test.lm$res, fit=test.lm$fit, dat), pch=16, cex=1, panel=panel.smooth, col.smooth="skyblue4", col=factor(dat$X7))

plot(Y ~ X1, data = dat, col=factor(X7))

curve(b[1] + b[2]*x, add=TRUE)
curve(b[1] + (b[2] + b[3])*x, add=TRUE, col=palette()[2])
curve(b[1] + (b[2] + b[4])*x, add=TRUE, col=palette()[3])

# pairs(cbind(R=test.lm$res, fit=test.lm$fit, dat), pch=16, cex=1, panel=panel.smooth, col.smooth="skyblue4", col=interaction(factor(dat$X1_1),factor(dat$X7)))
