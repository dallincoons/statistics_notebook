cars.lm <- lm(qsec ~ disp + I(disp^2) + am + disp:am + I(disp^2):am, data = mtcars)

b <- coef(cars.lm)

plot(qsec ~ disp, data=mtcars, col=c("skyblue", "firebrick")[factor(am)])

curve(b[1] + b[2]*x + b[3]*x^2, add = TRUE)
curve((b[1] + b[4]) + (b[2]*x + b[5]*x) + (b[3]*x^2 + b[6]*x^2), add = TRUE)
