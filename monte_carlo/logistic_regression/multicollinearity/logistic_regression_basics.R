# Load packages and functions ----
setwd("~/git/r_practice/monte_carlo/logistic_regression/multicollinearity/")
source('~/git/r_practice/monte_carlo/helper_functions/diagnostic_plots/estimate_histogram_generator.R')
source('~/git/r_practice/monte_carlo/helper_functions/diagnostic_plots/coverage_calculator.R')
source('~/git/r_practice/monte_carlo/helper_functions/diagnostic_plots/coverage_plot_generator.R')
library(tidyverse)
library(pracma)
library(mvtnorm)
library(ggcorrplot)
library(gt)

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

# Simulation: Basic model ----

# Initialization 
set.seed(12409)
# Set the number of reps of experiment
reps <- 1000
# Set sample size for each experiment
n <- 1000
# Set var names
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
# X  1  0  0
# X2 0  1  0
# X3 0  0  1
corr_levels = c(0, # X & X2
                0, # X & X3
                0) # X2 & X3
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

# Analyze results 
# Check for dropped vars from multicollinearity 
estimates_logit <- any_var_dropped(estimates_logit)
# Generate diagnostic plots
histograms_ideal <- plot_histograms(estimates_logit = estimates_logit
                    , true_values_logit = true_values_logit)
confidence_intervals_ideal <- plot_ci_coverage(estimates_logit = estimates_logit
                 , true_values_logit = true_values_logit)
# Generate correlation plot
named <- dat %>%
  as_tibble(.name_repair = 'universal') 
colnames(named) <- ind_vars  
correlation_plot_ideal <- named %>% 
  cor() %>%
  ggcorrplot(type = 'lower', 
             outline.color = 'white',
             lab = T, 
             ggtheme = theme_nothing) 
# Get descriptive stats about confidence interval size 
x1 <- calculate_coverage(estimates_logit$coeff_x1_est, 
                         estimates_logit$coeff_x1_se,
                         true_values_logit$coeff_x1_tru, 
                         level = .95, 
                         df = Inf)
ci_sizes <- x1$ci[,'upper_bound'] - x1$ci[,'lower_bound'] %>% 
  as.data.frame() 
# Generate Distribution Plot
ci_dist <- ci_sizes %>% ggplot + 
  geom_density(aes(x= .))
# Descriptive Stats
ci_sizes %>% summary()
# Save mean in variable for comparison to other simulations
ci_compare <- round(mean(pull(ci_sizes), na.rm = T), 2)

