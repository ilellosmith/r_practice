# Basic Logistic Regression Simulation 
# setwd("~/git/r_practice/monte_carlo")
# source('coverage_calculator')
source('estimate_histogram_generator.R')
source('coverage_calculator.R')
source('coverage_plot_generator.R')

library(tidyverse)
library(pracma)
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


set.seed(12409)

# Set the number of reps
reps <- 1000
# Create empty matrix to store estimates
estimates_logit <- tibble(
  intercept_est = numeric(length = reps)
  , coeff_x1_est = numeric(length = reps)
  , coeff_x2_est = numeric(length = reps)
  , intercept_se = numeric(length = reps)
  , coeff_x1_se = numeric(length = reps)
  , coeff_x2_se = numeric(length = reps)
)
# Define true values 
b0 <- 0.2
b1 <- 0.5
b2 <- 0.1
# Log true values in tibble
true_values_logit <- tibble(
  intercept_tru = b0
  , coeff_x1_tru = b1
  , coeff_x2_tru = b2
)
# Set sample size 
n <- 1000
# Generate observations of independent variable X
X <- runif(n, -1, 1)
X2 <- runif(n, -1, 1)


# ----------------------------------------------------------------------------

# Basic model

# ----------------------------------------------------------------------------

# Simulate models 
for (i in 1:reps){
  # Store value from true data generating process, Bernoulli trials
  Y <- rbinom(n, 1, inv_logit(b0+b1*X+b2*X2)) 
  # Estimate logit model for each independent value
  model <- glm(Y~X+X2, family = binomial(link = logit)) 
  # Calculate variance-covariance matrix for SEs
  vcv <- vcov(model) 
  # Store estimates and SEs
  estimates_logit$intercept_est[i] <- model$coef[1] # intercept
  estimates_logit$coeff_x1_est[i] <- model$coef[2] # coeff on X
  estimates_logit$coeff_x2_est[i] <- model$coef[3] # coeff on X2
  estimates_logit$intercept_se[i] <- sqrt(diag(vcv)[1]) # SE intercept
  estimates_logit$coeff_x1_se[i] <- sqrt(diag(vcv)[2]) # SE coefficient on X
  estimates_logit$coeff_x2_se[i] <- sqrt(diag(vcv)[3]) # SE coefficient on X2
}

estimates_logit <- any_var_dropped(estimates_logit)
  
plot_histograms(estimates_logit = estimates_logit
                    , true_values_logit = true_values_logit)

plot_ci_coverage(estimates_logit = estimates_logit
                 , true_values_logit = true_values_logit)


# ----------------------------------------------------------------------------

# Multi Collinearity 

# ----------------------------------------------------------------------------

# Set the number of reps
reps <- 1000
# Create empty matrix to store estimates
estimates_logit <- tibble(
  intercept_est = numeric(length = reps)
  , coeff_x1_est = numeric(length = reps)
  , coeff_x2_est = numeric(length = reps)
  , coeff_x3_est = numeric(length = reps)
  , intercept_se = numeric(length = reps)
  , coeff_x1_se = numeric(length = reps)
  , coeff_x2_se = numeric(length = reps)
  , coeff_x3_se = numeric(length = reps)
)
# Define true values 
b0 <- 0.2
b1 <- 0.5
b2 <- 0.1
b3 <- 0.7
# Log true values in tibble
true_values_logit <- tibble(
  intercept_tru = b0
  , coeff_x1_tru = b1
  , coeff_x2_tru = b2
  , coeff_x3_tru = b3
)
# Set sample size 
n <- 1000
# Generate observations of independent variable X
X <- runif(n, -1, 1)
X2 <- runif(n, -1, 1)
#X3 <- runif(n, 0, 2)*X2
X3 <- X2+rnorm(length(X2), 0, 1)
obs <- cbind(X,X2,X3) %>%
  as_tibble()

# Collinearity 
# Simulate models 
for (i in 1:reps){
  # Store value from true data generating process, Bernoulli trials
  Y <- rbinom(n, 1, inv_logit(b0+b1*X+b2*X2+b3*X3)) 
  # Estimate logit model for each independent value
  model <- glm(Y~X+X2+X3, family = binomial(link = logit)) 
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

estimates_logit <- any_var_dropped(estimates_logit)


plot_histograms(estimates_logit = estimates_logit
                    , true_values_logit = true_values_logit)

plot_ci_coverage(estimates_logit = estimates_logit
                 , true_values_logit = true_values_logit)

obs %>%
  filter(X3 < 1000) %>%
  ggplot(aes(x = X2, y = X3)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_cowplot()
