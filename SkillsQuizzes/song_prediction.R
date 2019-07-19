library(tidyverse)
library(scorecard)
library(pander)

songs <- read_csv('./../Data/song_attributes.csv')
songs$Y <- ifelse(songs$Like == 'Y', 1, 0)

songs$danceability_x <- songs$danceability > .5
songs$popularity_x <- songs$popularity > 50
songs$energy_x <- songs$energy > .5
songs$instrumentalness <- songs$instrumentalness > .5

lm <- glm(Y ~ popularity + instrumentalness + danceability_x + energy_x, data = songs, family = binomial)

summary(lm)

b <- coef(lm)

plot(Y ~ popularity, data = songs, pch=16)

instrumentalness = 0.2; danceable = 1; speechiness = 0.1; energetic = 0;
curve(1/(exp(-b[1] - b[2]*x - b[3]*instrumentalness - b[4]*danceable - b[5]*energetic) + 1), add = TRUE, col="black")

plot(Y ~ instrumentalness, data = songs, pch=16)

popularity = 40; instrumentalness = 0.2; danceable = 1; speechiness = 0.1; energetic = 0;
curve(1/(exp(-b[1] - b[2]*popularity - b[3]*x - b[4]*danceable - b[5]*energetic) + 1), add = TRUE, col="black")

# qt is used for condifence intervals
# -17 + c(-1,1)*qt(.975, 48)*.4155
# percentile is .975

# pt(t-value, 48)*2 gets you p-value

# predict(.. interval="prediction") to get confidence interval

# confidence deals with average, we're confident an average is within a certain range
# prediction is about individual value

# validate by getting a sample from original dataset, split into two datsets
# run glm on 

dt_list <- split_df(songs, ratio = 0.70, seed = 62)

v.glm <- glm(Y ~ popularity + instrumentalness + danceability_x + energy_x, data = dt_list$test, family = binomial)

probabilities <- predict(v.glm, dt_list$test, type="response")
predictions <- ifelse(probabilities > .5, "Y", "N")

pander(table(predictions, dt_list$test$Like))

(24+20)/50
