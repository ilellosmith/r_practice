# Basic Logistic Regression Simulation 
# setwd("~/git/r_practice/monte_carlo")
# source('coverage_calculator')
library(tidyverse)

#' Inverse Logit function
#' @param p logit  
#' @returns probability of 1 (or other value corresponding to logit)
inv_logit <- function(p){
  exp(p)/(1 + exp(p))
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
  
generate_histograms(estimates_logit = estimates_logit
                    , true_values_logit = true_values_logit)

