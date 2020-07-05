library(tidyverse)
# This script contains function definitions to complete the exercises in 
# Grinstead and Snell's "Introduction to Probability" (2006)

# Exercise 1 ----

#' coin_tosses tosses a coin n times and prints after every 100
#' tosses the proportion of heads minus 1/2 as well as the 
#' number of heads minus half the number of tosses
#' 
#' @param n number of coin tosses
coin_tosses <- function(n){
  tosses <- vector()
  for (i in 1:n){
    # coin toss, head is 1, tails is 0
    tosses[i] <- rbinom(1,1,0.5)
    # print values every 100 tosses
    if(i %% 100 == 0){
      print(paste('Proportion of heads above or below expected (0.5):', mean(tosses)-0.5, sep=''))
      print(paste('The number of heads above or below expected (', i/2, '): ', sum(tosses)-i/2, sep = ''))
    }
  }
}

# Exercise 2 ----

#' coin_tosses_delta repeats a simulation 100 times in which a coin is 
#' tossed n times. The function records for each experiment whether 
#' the proportion of heads is within 0.1 of 0.5 
#' 
#' @param n number of coin tosses
coin_tosses_delta <- function(n){
  tosses <- rbinom(100,n,0.5)/n
  delta_expect <- ifelse(tosses >= 0.4 & tosses <= 0.6, 1,0)
  # print(paste('Proportion of experiments where proportion heads between 0.4 and 0.6: ', mean(delta_expect), sep = ''))
  mean(delta_expect)
}

# Exercise 2 - Extension (I got tired of print statements)

#' mc_coin_tosses_delta repeats the coin_tosses_delta simulation 
#' n_trials times, at each of the number of coin tosses in n_tosses.
#' It then calculates the mean proportion of tosses for which
#' the proportion of heads is within 0.1 of 0.5 at each of the 
#' n_tosses values and plots tht proportion by n_tosses
#' 
#' @param n_trials number of trials at each level of tosses
#' @param n_tosses number of tosses in a given experiment
mc_coin_tosses_delta <- function(n_trials = 30, n_tosses = c(1:100)){
dat <- vector()
# Run experiments
for (i in 1:n_trials){
  trial <- sapply(n_tosses, coin_tosses_delta)
  dat <- cbind(trial, dat)
}
# Get mean of proportions at each n_tosses level
mc_prop <- rowMeans(dat) %>% 
  enframe()
colnames(mc_prop) <- c('n_tosses', 'prop')
# Plot mean of proportions at each n_tosses level against 0.95
mc_prop %>%
  ggplot(aes(x = n_tosses, y = prop)) +
  geom_point() +
  geom_hline(yintercept = 0.95, color = 'red') +
  theme_minimal() 
}




# Exercise 3 ----
#' Simulates the roll of three dice n times,
#' and tracks the proportion of times that their sum
#' is sum1 and sum2
#'
galileo_gamble <- function(n, sum1 = 9, sum2 = 10){
  # Simulate n sums from n rolls of three die
  roll_sums <- replicate(n, sum(sample(1:6, 3, replace = T)))
  # Count the number of times sum1 and sum2 occured
  sum1_n <- sum(ifelse(roll_sums == sum1, 1, 0))
  sum2_n <- sum(ifelse(roll_sums == sum2, 1, 0))
  # Output results
  cbind(sum1_n, sum1, sum2_n, sum2) %>% 
    as_tibble() 
}