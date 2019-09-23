# This file contains practice with the different distributions
# available in R to build intuition. 
# Some code taken from Carsey and Harden (2014)

# Set up environment ----
# load dependencies, installing if necessary
REQUIRED_PACKAGES <- c("tidyverse", 'cowplot')
package.check <- lapply(REQUIRED_PACKAGES, FUN = function(x) {
  if (! require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

set.seed(12345)

# Some fundamental R statistical vocabulary ----

# Prefixes:
# p == CDF
# d == PDF/PMF
# q == Quantile function
# r == random draw

# Distribution Suffix
# norm == normal 
# logis == logistic
# t == t
# f == F
# unif == uniform 
# pois = Poisson
# exp == exponential 
# chisq == X^2
# binom == binom 

# Sample from uniform dist ----
?runif()
# sample 1000 values between 0 and 1 from uniform dist
probs <-runif(1000, min = 0, max = 1)
# plot the probability distribution of these values 
probs %>% 
  enframe() %>%
  ggplot() +
  geom_density(aes(x = value)) +
  ggtitle('Density of 1000 Values Sampled from Uniform Distribution') +
  theme_minimal()

# Standard normal ----
# convert to standard normal and plot prob density
?qnorm()
probs %>%
  qnorm(mean = 0, sd = 1) %>%
  enframe() %>%
  ggplot() +
  geom_density(aes(x = value)) +
  ggtitle('Density of 1000 Values Sampled from Standard Normal Distribution') +
  theme_minimal()

# compare to 1000 values sampled directly from standard normal
rnorm(n = 1000, mean = 0, sd = 1) %>%
  enframe() %>%
  ggplot() +
  geom_density(aes(x = value)) +
  ggtitle('Density of 1000 Values Sampled Directly from Standard Normal Distribution') +
  theme_minimal()

# CDF 
probs %>%
  sort() %>%
  qnorm(mean = 0, sd = 1) %>%
  enframe() %>%
  mutate(prob_less = pnorm(value, mean = 0, sd =1)) %>%
  ggplot() +
  geom_step(aes(x = value, y = prob_less))

# QQplot ----
#     > QQplots are useful for understanding the distribution of the variable in the population
#     > You plot the quantiles for your sample data against the quantiles for theoretical distributions
#       to see which it best approximates. 
#     > See https://mgimond.github.io/ES218/Week06a.html#how_normal_q-q_plots_behave_in_the_face_of_skewed_data
#       for more information 

# compare data (sampled from normal) to normal distribution
probs %>%
  enframe() %>%
  mutate(prob = value,
         norm_q = qnorm(probs, mean = 0, sd = 1)) %>%
  ggplot(aes(sample = norm_q)) + 
  geom_qq(distribution = qnorm) +
  geom_qq_line(col = 'red')

# compare data (sampled from binomial distribution) to normal distribution
?qbinom()
probs %>%
  enframe() %>%
  mutate(prob = value,
         binom_q = qbinom(prob, size = length(prob), prob = 0.5)) %>%
  ggplot(aes(sample = binom_q)) + 
  geom_qq(distribution = qnorm) +
  geom_qq_line(col = 'red')

# compare data (sampled from binomial distribution) to binomial distribution
probs %>%
  enframe() %>%
  mutate(prob = value,
         binom_q = qbinom(prob, size = length(probs), prob = 0.5)) %>%
  ggplot(aes(sample = binom_q)) + 
  geom_qq(distribution = qbinom, dparams = list(size = length(probs), prob = 0.5)) +
  geom_abline(color = 'red')

# Logitic Distribution ----
# convert to logis and plot density
?qlogis()
probs %>%
  qlogis() %>%
  enframe() %>%
  ggplot() +
  geom_density(aes(x = value)) +
  ggtitle('Density of 1000 Values Sampled from Logistic Distribution') +
  theme_minimal()


# Student's t Distribution ----
# convert to t dist and plot density
?qt()
#' explore behavior of student t with different df
#' @probs is a vector of probabilities from uniform
#' @df is number of degress of freedom
plot_t_dist <- function(probs, df = 1){
  probs %>%
    qt(df = df) %>%
    enframe() %>%
    ggplot() +
    geom_density(aes(x = value)) +
    ggtitle(paste('Density of', length(probs), 'Values \n Sampled from t-Dist with', df, 'df', sep = ' ')) +
    theme_minimal() +
    theme(plot.title = element_text(size = 12))
}
# run plot_t_dist with different dfs
dfs <- c(999, 500, 20,10, 5, 1) %>% sort()
t_dists <- map(dfs, ~plot_t_dist(probs, .x))
# use cowplot to put charts on the same plot
plot_grid(plotlist= t_dists)

#
 
