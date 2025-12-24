library(ggplot2)
library(dplyr)

# -------------------------

# Quiz 2

# -------------------------

# Question 1 and 2
# Given the data sets give a p value for two sided hypothesis 
# test that B1 is 0 or not, and report estimate of residual standard deviation

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

summary(lm(y~x))

# Question 3 and 4
# 3 what is the lower end of the 95% confidence interval for the 
# expected mpg at the average weight?

# 4 what is the weight coefficient interpreted as? (est expected 
# change in mpg / 1k lb weight increase)
cars_fit <- lm(mpg ~ wt, mtcars)
summary(cars_fit)

# visual solution
mtcars %>% 
  ggplot(aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  geom_vline(xintercept = avg_wt)

new_wt <- data.frame(wt = mean(mtcars$wt))
#predict(cars_fit, newdata = new_wt, interval = 'predict')
# function solution
predict(cars_fit, newdata = new_wt, interval = 'confidence')

?mtcars

# Question 5
# new car weights 3k lbs, whats upper end of 95% prediction interval
new_wt_3k <- data.frame(wt = 3)

predict(cars_fit, newdata = new_wt_3k, interval = 'prediction')

# Question 6
# 95% confidence interval for expected change in mpg per 1 short ton increase in weight
# lower endpoint 
summary(cars_fit)
(-5.3445 - 2*0.5591)*2

# Question 7
# If my X from a linear regression is measured in centimeters and I convert it to meters what would happen to the slope coefficient?
# multiplied by 100

cars_fit_scale <- lm(mpg ~ I(wt/1000), mtcars)
summary(cars_fit_scale)

# Question 8
# how does shifting X by a constant c impact slope and intercept?
# B0 - cB1
cars_fit_shift <- lm(mpg ~ I(wt+ 5), mtcars)
summary(cars_fit_shift)

# visual solution
mtcars %>% 
  ggplot(aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  geom_vline(xintercept = avg_wt)

mtcars %>% 
  ggplot(aes(x = I(wt+ 5), y = mpg)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  geom_vline(xintercept = avg_wt)

# Question 9
# About what is the ratio of the the sum of the squared errors when 
# comparing a model with just an intercept (denominator) to the model with 
# the intercept and slope (numerator)?

summary(cars_fit)

# should be r squared
# come back to this - this is not right. I wonder if its 4
# because r squared is .75, which is the % of variation explained by the model

# Question 10
cars_fit_noint <- lm(mpg ~ 0 + wt, mtcars)

sum(cars_fit_noint$residuals)
sum(cars_fit$residuals)

