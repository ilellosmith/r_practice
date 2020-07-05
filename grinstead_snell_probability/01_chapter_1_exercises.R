library(tidyverse)
library(microbenchmark)
library(tictoc)
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
#' and tracks the number of times that their sum
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
# Exercise 4 ----

#' Plays many games of raquetball, and returns your average 
#' probability of winning. 
#' 
#' Rules of the game: when you serve
#' you have a "serve" chance of winning the point. 
#' When you return, you have a "return" chance of winning back the serve
#' A player wins a point only on their serve. A player wins the game
#' when they reach win points
#' 
#' @param n_games - number of games to simulate
#' @param serve - your probability of winning a point when serving
#' @param return - your probability of winning a point when returning
#' @param win - number of points to win
#' @param last_win - who starts serving. If 1 then you start serving
#' 
play_many_games <- function(n_games = 100, serve =0.6, return =0.5, win = 21, last_win = 1){
  outcomes <- replicate(n_games, play_game())
  mean(outcomes)
}

#' Plays a single game of raquetball, where when you serve
#' you have a "serve" chance of winning the point. 
#' When you return, you have a "return" chance of winning back the serve
#' A player wins a point only on their serve. A player wins the game
#' when they reach win points
#' 
#' @param serve - your probability of winning a point when serving
#' @param return - your probability of winning a point when returning
#' @param win - number of points to win
#' @param last_win - who starts serving. If 1 then you start serving
#' 
play_game <- function(serve =0.6, return =0.5, win = 21, last_win = 1){
  # Initialize game
  my_score <- 0
  opp_score <- 0
  # Play game to 21
  while(my_score < 21 & opp_score < 21) {
    # If won last point, you serve
    if (last_win == 1){
      # Win prob = serve probability
      point <- rbinom(1,1,serve)
      # If you lose, they serve
      if (point == 0){
        last_win <- 0
        # Otherwise you win, your score goes up, you continue to serve
      } else {
        my_score <- my_score + 1
      }
      # If you lost last point, opponent serves  
    } else {
      # Win prob = return probability
      point <- rbinom(1,1, return)
      # If you win, you get to serve again
      if (point == 1){
        last_win <- 1
        # If you lose, opponents score goes up, they continue to serve
      } else {
        opp_score <- opp_score + 1
      }
    }
  }
  # calculate whether you won the game
  ifelse(my_score == win, 1, 0)
}
# Exercise 5 ----

#' Simulates the roll of three dice n times,
#' and tracks whether at least one triple-six 
#' was rolled 
#' 
#' @param n number of rolls of three dice
triple_six <- function(n){
  # Simulate n sums from n rolls of three die
  roll_sums <- replicate(n, sum(sample(1:6, 3, replace = T)))
  # Count whether at least one triple six was rolled 
  any(ifelse(roll_sums == 18, T,F))
}

#' Simulates n_trials of rolling three dice n times,
#' and calculates the probability of at least one 
#' triple-six when three dice are rolled n times
#' 
#' NB: this function operates by filling in a prefab matrix
#' 
#' @param n_trials - number of trials to simulate at each level of n_rolls
#' @param n_rolls - number of rolls of three dice
mc_triple_six <- function(n_trials, n_rolls){
  dat <- matrix(, nrow = n_trials, ncol = length(n_rolls))
  # Run experiments
  for (i in 1:n_trials){
    dat[i,] <- sapply(n_rolls, triple_six)
  }  
  # Get proportion with triple sixes at each n_rolls level
  mc_prop <- colMeans(dat) %>% 
    enframe()
  colnames(mc_prop) <- c('n_rolls', 'prop')
  # Plot proportion with triple sixes at each n_rolls level against 0.5
  mc_prop %>%
    ggplot(aes(x = n_rolls, y = prop)) +
    geom_point() +
    geom_hline(yintercept = 0.5, color = 'red') +
    theme_minimal() 
}

#' Simulates n_trials of rolling three dice n times,
#' and calculates the probability of at least one 
#' triple-six when three dice are rolled n times
#' 
#' NB: this function operates by cbinding each trial
#' 
#' @param n_trials - number of trials to simulate at each level of n_rolls
#' @param n_rolls - number of rolls of three dice
mc_triple_six_cbind <- function(n_trials, n_rolls){
  dat <- vector()
  # Run experiments
  for (i in 1:n_trials){
    trial <- sapply(n_rolls, triple_six)
    dat <- cbind(dat,trial)
  }  
  # Get proportion with triple sixes at each n_rolls level
  mc_prop <- rowMeans(dat) %>% 
    enframe()
  colnames(mc_prop) <- c('n_rolls', 'prop')
  # Plot proportion with triple sixes at each n_rolls level against 0.5
  mc_prop %>%
    ggplot(aes(x = n_rolls, y = prop)) +
    geom_point() +
    geom_hline(yintercept = 0.5, color = 'red') +
    theme_minimal() 
}

# My simulations take a while to run. Using autobenchmark to compare 
time_compare <- microbenchmark::microbenchmark(
  'matrix' = {
    mc_triple_six(100, c(150:200))
  },
  'cbind' = {
    mc_triple_six_cbind(100, c(150:200))
  }
)
autoplot(time_compare)

# Looks like cbind is actually more efficient 
# Need to figure out another way to speed this up




