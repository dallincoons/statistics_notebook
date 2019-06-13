library(dplyr)
View(starwars)
star.lm <- lm(mass ~ height + species, data=starwars)
summary(star.lm)
