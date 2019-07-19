library(tidyverse)

grades <- read_csv('./../Data/Math425HistoricGrades.csv')

grades$Y <- grades$`Final Letter Grade` == "A"

plot(Y ~ `Final Exam Score`, data=grades)

grade_glm <- glm(Y ~ `Final Exam Score`, data = grades, family=binomial)
b <- coef(grade_glm)
curve(exp(b[1] + b[2]*x)/(1 + exp(b[1] + b[2]*x)), add = TRUE)
curve(1/(exp(-b[1] - b[2]*x) + 1), add = TRUE)
summary(grade_glm)

plot(Y ~ `Hard Work 2`, data=grades)

grade_glm <- glm(Y ~ `Hard Work 2`, data = grades, family=binomial)
b <- coef(grade_glm)
curve(exp(b[1] + b[2]*x)/(1 + exp(b[1] + b[2]*x)), add = TRUE)
curve(1/(exp(-b[1] - b[2]*x) + 1), add = TRUE)
summary(grades_glm)

grade_glm <- glm(Y ~ `Hard Work 2` * `Final Exam Score`, data = grades, family=binomial)
summary(grade_glm)

library(ResourceSelection)

hoslem.test(grade_glm$y, grade_glm$fitted.values)
