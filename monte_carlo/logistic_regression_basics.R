# Basic Logistic Regression Simulation 
# setwd("~/git/r_practice/monte_carlo")
# source('coverage_calculator')
source('estimate_histogram_generator.R')
source('coverage_calculator.R')
source('coverage_plot_generator.R')

library(tidyverse)
library(pracma)
library(corrplot)

#' Inverse Logit function
#' @param p logit  
#' @returns probability of 1 (or other value corresponding to logit)
#' 
inv_logit <- function(p){
  exp(p)/(1 + exp(p))
}

#' Checks if all values are NA
#' @param x vector of values  
#' @returns boolean - all values in vector are NA
#' 
all_na <- function(x){
  all(is.na(x))
}

#' Checks if any values are not NA
#' @param x vector of values  
#' @returns boolean - at least one value in vector is not NA
#' 
not_all_na <- Negate(all_na)

#' Checks if any estimates automatically dropped from collinearity
#' @param estimates_logit tibble: logistic regression coefficient estimates 
#' @returns estimates_logit_plot tibble - logistic regression coefficient estimates with
#' NAs removed
#' 
any_var_dropped <- function(estimates_logit){
  
  # get implicitly dropped vars
  na_cols <- estimates_logit %>% 
      select_if(all_na)
  
  # if there are implicitly dropped vars, throw warning, remove them 
  if(na_cols %>% length > 0){
    na_var_names <- unique(str_extract(colnames(na_cols), '.+(?=_est|_se)'))
    warning(
    sprintf('The model set all estimates for variable(s) %s to NA, suggesting
             high multicollinearity. These variable(s) will be dropped from graphics',
             na_var_names)
    )
    estimates_logit_plot <- estimates_logit %>% select_if(not_all_na)
  } else {
  # if no implicityly dropped vars, return full estimates  
    estimates_logit_plot <- estimates_logit
  }
  # return estimates for plotting   
  estimates_logit_plot
}

#' Returns two tibbles: empty tibble to hold simulated estimates, and populated 
#' tibble with true coefficient values  
#' 
#' This function takes in simulated observations, uses the variable
#' names and types to build out the columns of the tibble to hold simulated 
#' estimates. The logic handles numeric variables differently than categorical, 
#' the latter of which needs to be broken out into indicator variables 
#' 
#' @param vars_to_make tibble: table of simulated observations 
#' @param reps integer: number of repetitions for monte carlo
#' @param true_values tibble: true values for logistic regression coefficients 

#' @returns list: tibbles of estimates and true values 
#' 
#' 
make_sim_tbls <- function(simulated_dat, reps, true_values) {
  
  # get var names, types and values
  vars_to_make <- tibble(
    'var_name' = colnames(simulated_dat),
    'var_type' = map_chr(simulated_dat, class),
    # if numeric, no need to make indicator, otherwise, record levels
    'var_values' = map(simulated_dat, ~ if(class(.x) == 'numeric'){NA}else{levels(.x)})
  )
  
  # create a tibble containing the column names of the tibble that will hold the simulation's estimates and true values 
  my_var_names <- vars_to_make %>% 
    mutate(var_name = str_to_lower(var_name)) %>%
    # start with estimate names
    mutate(est_names = case_when(
      # numeric only needs one variable name
      var_type %in% c('numeric') ~ paste0('coeff_', var_name, '_est') %>% as_list(),
      # categorical needs to be split into indicator variables
      var_type %in% c('factor') ~ map2(var_values, var_name, ~paste0('coeff_', .y, '_', .x, '_est')) %>% as_list(),
      T ~ NA %>% as_list()
    )) %>% 
    # add intercept row
    add_row(var_name = 'intercept', var_type = 'numeric', var_values = list(NA), est_names = list('intercept_est'), .before = 1) %>%
    # then add se column names and true value column names
    mutate(se_names = map(est_names, ~str_replace(., 'est', 'se'))) %>%
    mutate(true_vals_names = map(est_names, ~str_replace(., 'est', 'tru')))
  
  # build tibble to hold estimates
  column_names <- c(unlist(my_var_names$est_names), unlist(my_var_names$se_names))
  estimates_logit <- tibble('dummy_col' = numeric(length = reps))
  # iterate over names to make cols of rep length filled with zeros
  for(i in 1: (length(column_names)-1)){ estimates_logit <- bind_cols(estimates_logit,'dummy_col' = numeric(length = reps) )}
  colnames(estimates_logit) <- column_names
  
  # build tibble to hold true values
  column_names <- unlist(my_var_names$true_vals_names)
  true_values_logit <- true_values
  colnames(true_values_logit) <- column_names
  
  # export the estimates and true values 
  list('estimates_logit' = estimates_logit
       , 'true_values_logit' = true_values_logit)
}

# set seed for replicability
set.seed(12409)

# Define true values 
b0 <- 0.2
b1 <- 0.5
b2 <- 0.1
# combine true values
true_values <- t(c(b0,b1,b2)) %>% as_tibble()

# Set the number of reps
reps <- 1000
# Set sample size for each rep 
n <- 1000

# Generate observations of independent variables
X1 <- runif(n, -1, 1)
X2 <- runif(n, -1, 1)
# combine variables
obs <- tibble(X1,X2) %>%
  # change character to factor and order by sorted strings
  mutate_if(is_character, as_factor) %>%
  mutate_if(is.factor, ~fct_relevel(.,sort))

# Create empty tibble to store estimates
sim_tbls <- make_sim_tbls(obs, reps, true_values)
estimates_logit <- sim_tbls$estimates_logit
# Log true values in tibble
true_values_logit <- sim_tbls$true_values_logit

# ----------------------------------------------------------------------------

# Basic model

# ----------------------------------------------------------------------------

# Simulate models 
for (i in 1:reps){
  # Store value from true data generating process, Bernoulli trials
  Y <- rbinom(n, 1, inv_logit(b0+b1*X1+b2*X2)) 
  # Estimate logit model for each independent value
  model <- glm(Y~X1+X2, family = binomial(link = logit)) 
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

# Define true values 
b0 <- 0.2
b1 <- 0.5
b2 <- 0.1
b3 <- 0.7
b4_01 <- 0.6 
b4_02 <- 0.3
b4_03 <- 0.25
# combine true values
true_values <- t(c(b0,b1,b2,b3,b4_01,b4_02, b4_03)) %>% as_tibble()

# set seed for replicability
set.seed(12409)

# Set the number of reps
reps <- 1000
# Set sample size for each rep 
n <- 1000

# Generate observations of independent variables 
X1 <- runif(n, -1, 1)
X2 <- runif(n, -1, 1)
#X3 <- runif(n, 0, 2)*X2
# X3 <- X2+rnorm(length(X2), 0, 1)
X3 <- runif(n, -1, 1)
X4 <- sample(c('category01', 'category02', 'category03'), n, replace = T) 
# combine variables
obs <- tibble(X1,X2,X3,X4) %>%
  # change character to factor and order by sorted strings
  mutate_if(is_character, as_factor) %>%
  mutate_if(is.factor, ~fct_relevel(.,sort))

# Create empty tibble to store estimates
sim_tbls <- make_sim_tbls(obs, reps, true_values)
estimates_logit <- sim_tbls$estimates_logit
# Log true values in tibble
true_values_logit <- sim_tbls$true_values_logit

# Collinearity 
# Simulate models 
for (i in 1:reps){
  # Store value from true data generating process, Bernoulli trials
  Y <- rbinom(n, 1, inv_logit(b0+
                                b1*X1+
                                b2*X2+
                                b3*X3+
                                b4_01*(ifelse(X4 == 'category01', 1, 0))+
                                b4_02*(ifelse(X4 == 'category02', 1, 0))+
                                b4_03*(ifelse(X4 == 'category03', 1, 0)))) 
  # Estimate logit model for each independent value
  model <- glm(Y~X1+X2+X3+X4, family = binomial(link = logit)) 
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

# what about uncertainty of the ultimate outcome? 
true_y <- ifelse(inv_logit(b0+b1*X+b2*X2+b3*X3) >= 0.50 , 1, 0) 
table(true_y == Y)

# visualize correlation 
dat <- cbind(X,X2,X3)
cor_mat <- cor(dat, method = "pearson", use = "complete.obs")
corrplot(cor_mat, type="upper", order="hclust",
         col=RColorBrewer::brewer.pal(n=8, name="RdYlBu"))


# TODO 1: automate model writing? At some point you need to specify inter-variable 
# relationships which would be complex enough as to not save you much time 
# that said, you could generate the the ifelse statements for cat
# TODO 2: automate the estimate and SE storage - how does DGP relate to lm?
