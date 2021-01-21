# Load packages and functions ----
setwd("~/git/r_practice/monte_carlo")
source('estimate_histogram_generator.R')
source('coverage_calculator.R')
source('coverage_plot_generator.R')
library(tidyverse)
library(pracma)
library(mvtnorm)
library(ggcorrplot)
library(gt)
library(scales)

# Define Helper functions ----
#' Inverse Logit function
#' @param p logit  
#' @returns probability of 1 (or other value corresponding to logit)
inv_logit <- function(p){
  exp(p)/(1 + exp(p))
}

#' Checks if all values are NA
#' @param x vector of values  
#' @returns boolean - all values in vector are NA
all_na <- function(x){
  all(is.na(x))
}

#' Checks if any values are not NA
#' @param x vector of values  
#' @returns boolean - at least one value in vector is not NA
not_all_na <- Negate(all_na)

#' Checks if any estimates automatically dropped from collinearity
#' @param estimates_logit tibble: logistic regression coefficient estimates 
#' @returns estimates_logit_plot tibble - logistic regression coefficient estimates with
#' NAs removed
any_var_dropped <- function(estimates_logit){
  na_cols <- estimates_logit %>% 
    select_if(all_na)
  
  if(na_cols %>% length > 0){
    na_var_names <- unique(str_extract(colnames(na_cols), '.+(?=_est|_se)'))
    warning(
      sprintf('The model set all estimates for variable(s) %s to NA, suggesting
             high multicollinearity. These variable(s) will be dropped from graphics',
              na_var_names)
    )
    estimates_logit_plot <- estimates_logit %>% select_if(not_all_na)
  } else {
    estimates_logit_plot <- estimates_logit
  }
  
  estimates_logit_plot
  
}


# Multi Collinearity: Low ----
# Initialization 
set.seed(12409)
# Set the number of reps of experiment
reps <- 1000
# Set sample size for each experiment
n <- 1000
# set var names
ind_vars <- c('X1', 'X2', 'X3')
# Create empty matrix to store estimates
estimates_logit <- tibble(
  intercept_est = numeric(length = reps),
  coeff_x1_est = numeric(length = reps),
  coeff_x2_est = numeric(length = reps),
  coeff_x3_est = numeric(length = reps),
  intercept_se = numeric(length = reps),
  coeff_x1_se = numeric(length = reps),
  coeff_x2_se = numeric(length = reps),
  coeff_x3_se = numeric(length = reps)
)
# Define true values and log in tibble
b0 <- 0.2
b1 <- 0.5
b2 <- 0.1
b3 <- -0.3
true_values_logit <- tibble(
  intercept_tru = b0,
  coeff_x1_tru = b1,
  coeff_x2_tru = b2,
  coeff_x3_tru = b3,
)
# Define correlation among independent variables 
#    X  X2  X3
# X  1  0.5  0.1
# X2 0.5 1 0.75
# X3 0.1 0.75 1
corr_levels = c(0.5, # X & X2
                0, # X & X3
                0.6) # X2 & X3
corr_mat <- matrix(c(1, corr_levels[1], corr_levels[2], 
                     corr_levels[1], 1, corr_levels[3],
                     corr_levels[2], corr_levels[3], 1), 
                   nrow = 3, ncol = 3)
# check that correlation matrix is positive definite 
assertthat::assert_that(min(eigen(corr_mat)[[1]]) > 0, msg = 'Correlation matrix is not positive definite. 
    Change correlation levels so that all eigenvalues are positive before proceeding.') 
# Generate observations of independent variable X
dat <- rmvnorm(n, mean = c(0,0,0), sigma = corr_mat) 
X1 <- dat[,1]
X2 <- dat[,2]
X3 <- dat[,3]

# Simulate models 
for (i in 1:reps){
  # Store value from true data generating process, Bernoulli trials
  Y <- rbinom(n, 1, inv_logit(b0+b1*X1+b2*X2+b3*X3)) 
  # Estimate logit model for each independent value
  model <- glm(Y~X1+X2+X3, family = binomial(link = logit)) 
  # Calculate variance-covariance matrix for SEs
  vcv <- vcov(model) 
  # Store estimates and SEs
  estimates_logit$intercept_est[i] <- model$coef[1] # intercept
  estimates_logit$coeff_x1_est[i] <- model$coef[2] # coeff on X
  estimates_logit$coeff_x2_est[i] <- model$coef[3] # coeff on X2
  estimates_logit$coeff_x3_est[i] <- model$coef[4] # coeff on X3
  estimates_logit$intercept_se[i] <- sqrt(diag(vcv)[1]) # SE intercept
  estimates_logit$coeff_x1_se[i] <- sqrt(diag(vcv)[2]) # SE coefficient on X
  estimates_logit$coeff_x2_se[i] <- sqrt(diag(vcv)[3]) # SE coefficient on X2
  estimates_logit$coeff_x3_se[i] <- sqrt(diag(vcv)[4]) # SE coefficient on X3
}

# Check for dropped vars from multicollinearity 
estimates_logit <- any_var_dropped(estimates_logit)


# Plot diagnostic plots
histograms_low <- plot_histograms(estimates_logit = estimates_logit
                                    , true_values_logit = true_values_logit)

confidence_intervals_low <- plot_ci_coverage(estimates_logit = estimates_logit
                                               , true_values_logit = true_values_logit)

# Plot correlation matrix
named <- dat %>%
  as_tibble(.name_repair = 'universal') 
colnames(named) <- ind_vars  
correlation_plot_low <- named %>% 
  cor() %>%
  ggcorrplot(type = 'lower', 
             outline.color = 'white',
             lab = T, 
             ggtheme = theme_nothing) 

# Get Confidence Interval Size 
x1_b <- calculate_coverage(estimates_logit$coeff_x1_est, 
                         estimates_logit$coeff_x1_se,
                         true_values_logit$coeff_x1_tru, 
                         level = .95, 
                         df = Inf)
ci_sizes_b <- x1_b$ci[,'upper_bound'] - x1_b$ci[,'lower_bound'] %>% 
  as.data.frame() 
# Generate Distribution Plot
ci_sizes_b %>% ggplot + 
  geom_density(aes(x= .))
# Descriptive Stats
ci_sizes_b %>% summary()
# Save mean in variable for comparison to other simulations
ci_compare_b <- round(mean(pull(ci_sizes_b), na.rm = T), 2)


# Multi Collinearity: High ----
# Initialization 
set.seed(11309)
# Set the number of reps of experiment
reps <- 1000
# Set sample size for each experiment
n <- 1000
# set var names
ind_vars <- c('X1', 'X2', 'X3')
# Create empty matrix to store estimates
estimates_logit <- tibble(
  intercept_est = numeric(length = reps),
  coeff_x1_est = numeric(length = reps),
  coeff_x2_est = numeric(length = reps),
  coeff_x3_est = numeric(length = reps),
  intercept_se = numeric(length = reps),
  coeff_x1_se = numeric(length = reps),
  coeff_x2_se = numeric(length = reps),
  coeff_x3_se = numeric(length = reps)
)
# Define true values and log in tibble
b0 <- 0.2
b1 <- 0.5
b2 <- 0.1
b3 <- -0.3
true_values_logit <- tibble(
  intercept_tru = b0,
  coeff_x1_tru = b1,
  coeff_x2_tru = b2,
  coeff_x3_tru = b3,
)
# Define correlation among independent variables 
#    X  X2  X3
# X  1  0.5  0.1
# X2 0.5 1 0.75
# X3 0.1 0.75 1
corr_levels = c(0.1, # X & X2
                    0.96, # X & X3
                    0.2) # X2 & X3
corr_mat <- matrix(c(1, corr_levels[1], corr_levels[2], 
                     corr_levels[1], 1, corr_levels[3],
                     corr_levels[2], corr_levels[3], 1), 
                   nrow = 3, ncol = 3)
# Check that correlation matrix is positive definite 
assertthat::assert_that(min(eigen(corr_mat)[[1]]) > 0, msg = 'Correlation matrix is not positive definite. 
    Change correlation levels so that all eigenvalues are positive before proceeding.') 
# Generate observations of independent variable X
dat <- rmvnorm(n, mean = c(0,0,0), sigma = corr_mat) 
X1 <- dat[,1]
X2 <- dat[,2]
X3 <- dat[,3]

# Simulate models 
for (i in 1:reps){
  # Store value from true data generating process, Bernoulli trials
  Y <- rbinom(n, 1, inv_logit(b0+b1*X1+b2*X2+b3*X3)) 
  # Estimate logit model for each independent value
  model <- glm(Y~X1+X2+X3, family = binomial(link = logit)) 
  # Calculate variance-covariance matrix for SEs
  vcv <- vcov(model) 
  # Store estimates and SEs
  estimates_logit$intercept_est[i] <- model$coef[1] # intercept
  estimates_logit$coeff_x1_est[i] <- model$coef[2] # coeff on X
  estimates_logit$coeff_x2_est[i] <- model$coef[3] # coeff on X2
  estimates_logit$coeff_x3_est[i] <- model$coef[4] # coeff on X3
  estimates_logit$intercept_se[i] <- sqrt(diag(vcv)[1]) # SE intercept
  estimates_logit$coeff_x1_se[i] <- sqrt(diag(vcv)[2]) # SE coefficient on X
  estimates_logit$coeff_x2_se[i] <- sqrt(diag(vcv)[3]) # SE coefficient on X2
  estimates_logit$coeff_x3_se[i] <- sqrt(diag(vcv)[4]) # SE coefficient on X3
}

# Check for dropped vars from multicollinearity 
estimates_logit <- any_var_dropped(estimates_logit)


# Plot diagnostic plots
histograms_high <- plot_histograms(estimates_logit = estimates_logit
                                  , true_values_logit = true_values_logit)

confidence_intervals_high <- plot_ci_coverage(estimates_logit = estimates_logit
                                             , true_values_logit = true_values_logit)

# Plot correlation matrix
named <- dat %>%
  as_tibble(.name_repair = 'universal') 
colnames(named) <- ind_vars  
correlation_plot_high <- named %>% 
  cor() %>%
  ggcorrplot(type = 'lower', 
             outline.color = 'white',
             lab = T, 
             ggtheme = theme_nothing) 

# Get Confidence Interval Size 
x1_c <- calculate_coverage(estimates_logit$coeff_x1_est, 
                           estimates_logit$coeff_x1_se,
                           true_values_logit$coeff_x1_tru, 
                           level = .95, 
                           df = Inf)
ci_sizes_c <- x1_c$ci[,'upper_bound'] - x1_c$ci[,'lower_bound'] %>% 
  as.data.frame() 
# Generate Distribution Plot
ci_sizes_c %>% ggplot + 
  geom_density(aes(x= .))
# Descriptive Stats
ci_sizes_c %>% summary()
# Save mean in variable for comparison to other simulations
ci_compare_c <- round(mean(pull(ci_sizes_c), na.rm = T), 2)

# Multicollinearity threshold ----
# Initialization 
set.seed(10239)
# Set the number of reps of experiment
reps <- 1000
# Set sample size for each experiment
n <- 1000
# Set var names
ind_vars <- c('X1', 'X2', 'X3')
# Define true values and log in tibble
b0 <- 0.2
b1 <- 0.5
b2 <- 0.1
b3 <- -0.3
true_values_logit <- tibble(
  intercept_tru = b0,
  coeff_x1_tru = b1,
  coeff_x2_tru = b2,
  coeff_x3_tru = b3,
)
# Define correlation levels to iterate over 
mc_level_X2_X3 <- c(seq(0,0.9,0.1), 0.99)
# Create empty matrix to store standard deviations of the estimates 
sd_betas <- tibble(
  coeff_x1_sd = numeric(length = length(mc_level_X2_X3)),
  coeff_x2_sd = numeric(length = length(mc_level_X2_X3)),
  coeff_x3_sd = numeric(length = length(mc_level_X2_X3))
)
# Iterate over mc_levels, conducting 1000 experiments at each level 
for (j in 1:length(mc_level_X2_X3)) { 
  # Define correlation among independent variables 
  corr_levels = c(0.2, # X & X2
                  0.3, # X & X3
                  mc_level_X2_X3[j]) # X2 & X3
  corr_mat <- matrix(c(1, corr_levels[1], corr_levels[2], 
                       corr_levels[1], 1, corr_levels[3],
                       corr_levels[2], corr_levels[3], 1), 
                     nrow = 3, ncol = 3)
  # Check that correlation matrix is positive definite 
  assertthat::assert_that(min(eigen(corr_mat)[[1]]) > 0, msg = 'Correlation matrix is not positive definite. 
    Change correlation levels so that all eigenvalues are positive before proceeding.') 
  # Generate observations of independent variable X
  dat <- rmvnorm(n, mean = c(0,0,0), sigma = corr_mat) 
  X1 <- dat[,1]
  X2 <- dat[,2]
  X3 <- dat[,3]
  # Create tibble to store estimates 
  estimates_logit <- tibble(
    intercept_est = numeric(length = reps),
    coeff_x1_est = numeric(length = reps),
    coeff_x2_est = numeric(length = reps),
    coeff_x3_est = numeric(length = reps),
    intercept_se = numeric(length = reps),
    coeff_x1_se = numeric(length = reps),
    coeff_x2_se = numeric(length = reps),
    coeff_x3_se = numeric(length = reps)
  )
  # Simulate models 
  for (i in 1:reps){
    # Store value from true data generating process, Bernoulli trials
    Y <- rbinom(n, 1, inv_logit(b0+b1*X1+b2*X2+b3*X3)) 
    # Estimate logit model for each independent value
    model <- glm(Y~X1+X2+X3, family = binomial(link = logit)) 
    # Calculate variance-covariance matrix for SEs
    vcv <- vcov(model) 
    # Store estimates and SEs
    estimates_logit$intercept_est[i] <- model$coef[1] # intercept
    estimates_logit$coeff_x1_est[i] <- model$coef[2] # coeff on X
    estimates_logit$coeff_x2_est[i] <- model$coef[3] # coeff on X2
    estimates_logit$coeff_x3_est[i] <- model$coef[4] # coeff on X3
    estimates_logit$intercept_se[i] <- sqrt(diag(vcv)[1]) # SE intercept
    estimates_logit$coeff_x1_se[i] <- sqrt(diag(vcv)[2]) # SE coefficient on X
    estimates_logit$coeff_x2_se[i] <- sqrt(diag(vcv)[3]) # SE coefficient on X2
    estimates_logit$coeff_x3_se[i] <- sqrt(diag(vcv)[4]) # SE coefficient on X3
  }  
  sd_betas[j,] <- t(c(sd(estimates_logit$coeff_x1_est),
                    sd(estimates_logit$coeff_x2_est),
                    sd(estimates_logit$coeff_x3_est)))
  cat('Completed simulation of correlation:', mc_level_X2_X3[j], ", simulation #", j, 
          "of", length(mc_level_X2_X3), ")\n")
}

sd_change <- sd_betas %>% 
  mutate(mc_level = mc_level_X2_X3) %>%
  pivot_longer(-mc_level, names_to = 'coefficient', values_to = 'sd') %>%
  mutate(coefficient = str_to_lab(str_extract(coefficient, '[a-z]+_x[0-9]'))) %>%
  ggplot() + 
  geom_line(aes(x = mc_level, y = sd, color = coefficient),
            size = 1.5,
            position = position_dodge2(width = 0.1)) + 
  scale_color_manual(values = c('Coeff X1' = 'black',
                     'Coeff X2' = 'darkred', 
                     'Coeff X3' = '#CC0000'))  +
  scale_x_continuous(breaks = mc_level_X2_X3) +
  labs(color = 'Coefficient') +
  xlab('\n Correlation between X2 and X3') + 
  ylab(paste('Standard deviation of \n coefficient estimates \n across', reps, 'experiments    \n')) +
  theme_cowplot() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) 

  
# Future questions: 
# - Learn more about positive definite - why can't cov matrixes on real data 
# be non positive definite? 

  
# - vary the amount of correlation (between 2 or 3 vars)? 
# - let's do three but only two are really correlated. 
# Start at zero, go to 1 for the two correlated. 
# - plot standard deviation of B estimates for each of the three 
# variables as you increase correlation. threshold for one or the other?
# does one stay ok throughout?

# 
# # new function to calculate distribution stats for size of CIs
# 
# plot_histograms(estimates_logit = estimates_logit %>% select(-c(coeff_x3_est, coeff_x3_se)),
#                 true_values_logit = true_values_logit %>% select(-coeff_x3_tru))
# 
# plot_histograms(estimates_logit = estimates_logit
#                 , true_values_logit = true_values_logit)
# 
# plot_ci_coverage(estimates_logit = estimates_logit %>% select(-c(coeff_x3_est, coeff_x3_se)),
#                  true_values_logit = true_values_logit %>% select(-coeff_x3_tru))

# with_multi1 <- mean(estimates_logit$coeff_x1_se)
# with_multi2 <- mean(estimates_logit$coeff_x2_se)
# with_multi3 <- mean(estimates_logit$coeff_x3_se)
# 
# without_multi1 <- mean(estimates_logit$coeff_x1_se)
# without_multi2 <- mean(estimates_logit$coeff_x2_se)
# without_multi3 <- mean(estimates_logit$coeff_x3_se)


# adding multicollinearity makes SE larger, but CI coverage doesn't drop 
# also makes the CIs larger s.t. they maintain coverage

# adding endogeneity introduces bias in other estimates 
# what would be helpful is to quantify the increase in uncertainty 
# with multicollinearity 
# How to remove the Coeff X3? 

# corr_obs %>%
#   filter(X3 < 1000) %>%
#   ggplot(aes(x = X2, y = X3)) +
#   geom_point() +
#   geom_smooth(method = 'lm') +
#   theme_cowplot()

#https://www.r-bloggers.com/2015/03/simulating-endogeneity/

# For MPR - 
# example with no correlation (meets all assumptions)
# example with correlation - look up simulating multivariate correlation
# Save the categorical for another time 

# plot_average
# 
# summarize_se <- estimates_logit %>% 
#   select_if(str_detect(colnames(.),'se')) %>%
#   map(., ~summary(.x))
# 
# cover_probs <- calculate_coverage(coeff_est, coeff_se, true_value, level, df)
# 
# 
# summarize_se$intercept_se['Mean']
