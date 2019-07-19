library(mosaic)
library(plotly)

u.lm <- lm(elecbill ~ year + temp + I(temp^2), data=Utilities[-95, ])

b <- coef(u.lm)

par(mfrow=c(2,1))
plot(elecbill ~ year, data=Utilities[-95, ])

temp = 9
curve(b[1] + b[2]*x + b[3]*temp + b[4]*temp^2, add=TRUE)

temp = 50.5
curve(b[1] + b[2]*x + b[3]*temp + b[4]*temp^2, add=TRUE)

temp = 78
curve(b[1] + b[2]*x + b[3]*temp + b[4]*temp^2, add=TRUE)

plot(elecbill ~ temp, data=Utilities[-95, ])

year = 1999
curve(b[1] + b[2]*year + b[3]*x + b[4]*x^2, add=TRUE)

year = 2005
curve(b[1] + b[2]*year + b[3]*x + b[4]*x^2, add=TRUE)

year = 2010
curve(b[1] + b[2]*year + b[3]*x + b[4]*x^2, add=TRUE)

summary_temp <- summary(Utilities[-95, ]$temp)
summary_year <- summary(Utilities[-95, ]$year)

tempX <- 0



tempX <- summary_temp['Mean']
curve(b[1] + b[2]*x + b[3]*x*tempX + b[4]*x^2*tempX, add=TRUE)
