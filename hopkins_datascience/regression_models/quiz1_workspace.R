library(ggplot2)
library(dplyr)
library(purrr)

# -------------------------

# Quiz 1 

# -------------------------

# Question 1
# Given the data sets and weights below, what is the value of
# mu that minimizes least squares
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

# I would've though it was the mean, but 
# the weights change the picture 
mean_x <- mean(x)

option_a <- mean_x
option_b <- 0.1471
option_c <- 1.077
option_d <- 0.3

options <- c(option_a, option_b, option_c, option_d)
outcomes_unweighted <- unlist(map(options, ~sum((x - .x)^2)))
outcomes_weighted <- unlist(map(options, ~sum(w*(x - .x)^2)))
sim <- tibble(options, outcomes_unweighted, outcomes_weighted)

min_mu_unweighted <- filter(sim, outcomes_unweighted == min(outcomes_unweighted))
min_mu_weighted <- filter(sim, outcomes_weighted == min(outcomes_weighted))

# Question 2
# This one was tricky. They asked for regression through the origin, which means 
# setting the intercept to 0, but not centering the data. 

# Not clear if they taught this in the course? In any case does not seem to be 
# something I'd be reaching for often
# https://stats.stackexchange.com/questions/7948/when-is-it-ok-to-remove-the-intercept-in-a-linear-regression-model

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

dat <- data.frame(x,y)
dat <- dat %>% 
  mutate(
    x_norm = scale(x), 
    y_norm = scale(y),
    x_scale = scale(x, center = F), 
    y_scale = scale(y, center = F),
    x_center = scale(x, scale = F), 
    y_center = scale(y, scale = F)
  )

dat %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0)) + 
  geom_vline(aes(xintercept = 0)) + 
  geom_smooth(method = 'lm', formula = y~-1+x)

dat %>% 
  ggplot(aes(x = x_center, y = y_center)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0)) + 
  geom_vline(aes(xintercept = 0)) + 
  geom_smooth(method = 'lm')

mean(dat$x_center)
mean(dat$y_center)

dat %>% 
  ggplot(aes(x = x_scale, y = y_scale)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0)) + 
  geom_vline(aes(xintercept = 0)) + 
  geom_smooth(method = 'lm')

dat %>% 
  ggplot(aes(x = x_norm, y = y_norm)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0)) + 
  geom_vline(aes(xintercept = 0)) + 
  geom_smooth(method = 'lm')

model_no_int <- lm(dat$y ~ -1 + dat$x)
summary(model_no_int)

#Question 3
data(mtcars)
model_cars <- lm(mtcars$mpg ~ mtcars$wt)
summary(model_cars) # they ask for the slope coefficient which is -5.34

#Question 4
# beta = cor(y,x) * sd(y)/sd(x) for Y predicted by X
# beta = cor(y,x) * sd(x)/sd(y) for X predicted by Y
# cor(y,x) == cor(x,y) for all cases

# we know sd(y)/sd(x) = 1/0.5 = 2 
# we know cor(y,x) = 0.5

# they are asking for beta = cor(y,x) * sd(y)/sd(x) for Y predicted by X
# that is 0.5 * 2 = 1

# need to go back to the course material to really understand the proofs

#Question 5
# students get two quizes, scores normalized to mean 0 variance 1. 
# correlation between scores was 0.4. Student got normalized score 1.5 on Quiz 1
# so Quiz 2 is what
1.5*0.4

#Question 6 
# what is value of first measurement if x normalized? 
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x - mean(x))/sd(x)
scale(x)

#Question 7
# what is intercept for y ~ x
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
model <- lm(y ~ x)
summary(model)

#Question 8
# regression line runs through the means of both predictor and response

# Question 9
# this is the unweighted case of minimizing residuals. the mean does it
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
sum((x - mean(x))^2)
sum((x - mean(x)*1.1)^2)
sum((x - mean(x)*.9)^2)
mean(x)

# Question 10
# what is the ratio of the slopes from y ~ x and x ~ y always equal to? 

# cor(y,x) * sd(y)/sd(x) / cor(y,x) * sd(x)/sd(y)
# correlation cancels 
# sd(y)/sd(x) * sd(y)/sd(x)
# sd(y)^2 / sd(x)^2 
# var(y) / var(x)

model_inv <- lm(x ~y)

slope_model = model$coefficients[2]
slope_model_inv = model_inv$coefficients[2]

slope_ratio <- slope_model / slope_model_inv
slope_ratio

var(y)/var(x) - slope_ratio
2*sd(y)/sd(x) - slope_ratio
cor(y,x) - slope_ratio
