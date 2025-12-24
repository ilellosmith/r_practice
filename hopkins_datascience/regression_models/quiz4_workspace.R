library(ggplot2)
library(dplyr)
library(MASS)
library(splines)

# -------------------------

# Quiz 4

# -------------------------

# Question 1 

# Consider the space shuttle data ?shuttle in the MASS library.
# Consider modeling the use of the autolander as the outcome 
# (variable name use). Fit a logistic regression model with 
# autolander (variable auto) use (labeled as "auto" 1) versus not (0) 
# as predicted by wind sign (variable wind). 
# Give the estimated odds ratio for autolander use comparing head winds, 
# labeled as "head" in the variable headwind (numerator) to tail winds (denominator).

head(shuttle)

shuttle <- shuttle %>% 
  mutate(use_binary = ifelse(use == 'auto', 1, 0))

shuttle_mod_ref_head <- glm(use_binary ~ I(relevel(wind, ref = 'tail')), family = 'binomial', data = shuttle)
shuttle_mod <- glm(use_binary ~ wind, family = 'binomial', data = shuttle)
summary(shuttle_mod)
summary(shuttle_mod_ref_head)

exp(coefficients(shuttle_mod_ref_head))

odds_auto_head_ref <- exp(shuttle_mod_ref_head$coefficients[1] + shuttle_mod_ref_head$coefficients[2])
odds_auto_tail_ref <- exp(shuttle_mod_ref_head$coefficients[1])
odds_auto_head_ref / odds_auto_tail_ref

odds_auto_head <- exp(shuttle_mod$coefficients[1])
odds_auto_tail <- exp(shuttle_mod$coefficients[1] + shuttle_mod$coefficients[2])
odds_auto_head / odds_auto_tail

# Question 2

# Consider the previous problem. Give the estimated odds ratio for autolander 
#use comparing head winds (numerator) to tail winds (denominator) adjusting 
#for wind strength from the variable magn.

shuttle_mod_mag <- glm(use_binary ~ wind + magn, family = 'binomial', data = shuttle)

summary(shuttle_mod_mag) 

odds_auto_head <- exp(shuttle_mod_mag$coefficients[1])
odds_auto_tail <- exp(shuttle_mod_mag$coefficients[1] + shuttle_mod_mag$coefficients[2])
odds_auto_head / odds_auto_tail

# Question 3

# If you fit a logistic regression model to a binary variable, 
# for example use of the autolander, then fit a logistic regression model 
# for one minus the outcome (not using the autolander) 
# what happens to the coefficients?

shuttle <- shuttle %>% 
  mutate(use_binary_inv = ifelse(use == 'auto', 0, 1))

shuttle_mod_inv <- glm(use_binary_inv ~ wind, family = 'binomial', data = shuttle)

shuttle_mod_inv$coefficients
shuttle_mod$coefficients

# Question 4

# Consider the insect spray data InsectSprays. Fit a Poisson model using 
# spray as a factor level. Report the estimated relative rate 
# comparing spray A (numerator) to spray B (denominator).

insect_mod <- glm(count ~ spray,  family = 'poisson', data = InsectSprays)
insect_mod

exp(insect_mod$coefficients[1])/ exp(insect_mod$coefficients[1] + insect_mod$coefficients[2])

exp(2.67415)/exp(2.67415+0.05588)

# Question 5
# Consider a Poisson glm with an offset, t. So, for example, a model of the form 
# glm(count ~ x + offset(t), family = poisson) where x is a factor variable 
# comparing a treatment (1) to a control (0) and t is the natural log of 
# a monitoring time. What is impact of the coefficient for x if we fit the 
# model glm(count ~ x + offset(t2), family = poisson)
# where t2 <- log(10) + t. In other words, what happens to the coefficients 
# if we change the units of the offset variable. 
# (Note, adding log(10) on the log scale is multiplying by 10 
# on the original scale.)

# adding the offset is like dividing by the unit time. 
# so, what if we did weeks instead of days for our unit?
# we'd expect more events within a time period. While the 
# rate shouldn't change, we should see the intercept increase 
# in order to deliver the same rate of events in fewer time intervals. 

# Question 6

# Consider the data

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
dat <- tibble(x = x,y = y)

# Using a knot point at 0, fit a linear model that looks like a hockey stick 
# with two lines meeting at x=0. Include an intercept term, x and the 
# knot point term. What is the estimated slope of the line after 0?

plot(x,y)

# basis point solution from coursera
z <- (x>0) * x
fit <- lm(y ~ x + z)
coef(fit)
sum(coef(fit)[2:3])

# more general solution 
fit_bs <- lm(y ~ bs(x, knots = 0, degree = 1))
summary(fit_bs)

# degree = 1 means linear
# knots = 0 means one break at 0

p <- predict(fit_bs, newdata = data.frame(x = c(1, 2)))
slope_after_0 <- p[2] - p[1]
