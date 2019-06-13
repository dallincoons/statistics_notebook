## Simulating Data from a Regression Model
## This R-chunk is meant to be played in your R Console.
## It allows you to explore how the various elements
## of the regression model combine together to "create"
## data and then use the data to "re-create" the line.

set.seed(101) #Allows us to always get the same "random" sample
#Change to a new number to get a new sample

n <- 30 #set the sample size

X_i <- runif(n, 15, 45)
#Gives n random values from a uniform distribution between 15 to 45.

beta0 <- -1000 #Our choice for the y-intercept. 
beta1 <- 75 #Our choice for the slope. 
beta2 <- -1
sigma <- 55 #Our choice for the std. deviation of the error terms.


epsilon_i <- rnorm(n, 0, sigma) 
#Gives n random values from a normal distribution with mean = 0, st. dev. = sigma.

Y_i <- beta0 + beta1*X_i + beta2*X_i^2 + epsilon_i
#Create Y using the normal error regression model

fabData <- data.frame(y=Y_i, x=X_i)

#In the real world, we begin with data (like fabData) and try to recover the model that 
# (we assume) was used to created it.

fab.lm <- lm(y ~ x + I(x^2), data=fabData) #Fit an estimated regression model to the fabData.

b <- coef(fab.lm)

summary(fab.lm) #Summarize your model. 

plot(y ~ x, data=fabData) #Plot the data.
curve(beta0 + beta1 * x + beta2 * x^2, lty=2, add = TRUE)
curve(b[1] + b[2] * x + b[3] * x^2, lty=1, add = TRUE)


beta0 <- 2 #Our choice for the y-intercept. 
beta1 <- 65 #Our choice for the slope. 
beta2 <- -2
sigma <- 250 #Our choice for the std. deviation of the error terms.

beta0_2 <- 2 #Our choice for the y-intercept. 
beta1_2 <- 35 #Our choice for the slope. 
beta2_2 <- -2
sigma_2 <- 150 #Our choice for the std. deviation of the error terms.

X_i <- runif(n, 15, 45)
X_i2 <- runif(n, 15, 45)

epsilon_i <- rnorm(n, 0, sigma) 
epsilon_i2 <- rnorm(n, 0, sigma_2)

Y_i <- beta0 + beta1*X_i + epsilon_i
Y_i2 <- beta0_2 + beta1_2*X_i2 + epsilon_i2

fabData <- data.frame(y=Y_i, x=X_i, type='one')
fabData <- rbind(fabData, data.frame(y=Y_i2, x=X_i2, type='two'))

plot(y ~ x, data=fabData, col=c("skyblue", "orange")[as.factor(type)], pch=21, cex.main=1)

mylm <- lm(y ~ x + type + x:type, data=fabData)

b <- coef(mylm)

curve(b[1] + b[2]*x, col="skyblue", lty = 2, add=TRUE)
curve((b[1] + b[3]) + (b[2] + b[4])*x, col="orange", lty = 2, add=TRUE)
curve(beta0 + beta1*x, col="skyblue", lty = 1, add=TRUE)
curve(beta0_2 + beta1_2*x, col="orange", lty = 1, add=TRUE)

beta0 <- 2 #Our choice for the y-intercept. 
beta1 <- 3 #Our choice for the slope. 
beta2 <- 5
beta3 <- -1
sigma <- 4050 #Our choice for the std. deviation of the error terms.

X_i <- runif(n, 15, 45)

epsilon_i <- rnorm(n, 0, sigma)

Y_i <- beta0 + beta1*X_i + beta2*X_i^2 + beta3*X_i^3 + epsilon_i

bands <- data.frame(valence = Y_i, bpm = X_i)

plot(valence ~ bpm, data = bands)

bandslm <- lm(valence ~ bpm + I(bpm^2) + I(bpm^3), data=bands)
b <- coef(bandslm)

curve(b[1] + b[2]*x + b[3]*x^2 + b[4]*x^3, lty =2, add = TRUE)
curve(beta0 + beta1*x + beta2*x^2 + beta3*x^3, lty = 1, add = TRUE)
