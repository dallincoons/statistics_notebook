YoungOrange <- Orange %>% filter(age < 1200)


log.lm <- lm(log(circumference) ~ age, YoungOrange)

ggplot(YoungOrange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  stat_function(fun = function(x) exp(log.lm$coefficients[1] + log.lm$coefficients[2] * x))
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw()

plot(circumference ~ age, data=YoungOrange, pch=16, col="orangered", main="Growth of Orange Trees", xlab="Age of Tree in Days", ylab="Circumference of Tree (mm)")
curve(exp(log.lm$coefficients[1] + log.lm$coefficients[2] * x), add=TRUE)

sqrt.lm <- lm(sqrt(circumference) ~ age, Orange)

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  stat_function(fun = function(x) (sqrt.lm$coefficients[1] + sqrt.lm$coefficients[2] * x)^2)
labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw()

recip.lm <- lm(1/(circumference) ~ age, Orange)

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  stat_function(fun = function(x) 1/(recip.lm$coefficients[1] + recip.lm$coefficients[2] * x))
labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw()

normal.lm <- lm(circumference ~ age, Orange)

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  stat_function(fun = function(x) normal.lm$coefficients[1] + normal.lm$coefficients[2] * x)
labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw()

square.lm <- lm((circumference^2) ~ age, Orange)

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  stat_function(fun = function(x) sqrt(square.lm$coefficients[1] + square.lm$coefficients[2] * x))
labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw()

mystery.lm <- lm((circumference)^3 ~ age, Orange)

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  stat_function(fun = function(x) sqrt(mystery.lm$coefficients[1] + mystery.lm$coefficients[2] * x))
labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw()
