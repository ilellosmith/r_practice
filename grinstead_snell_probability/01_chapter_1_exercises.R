library(tidyverse)
library(microbenchmark)
library(tictoc)
library(scales)
library(boot)
library(parallel)
library(foreach)
library(doParallel)
library(arrangements)
# This script contains function definitions to complete the exercises in 
# Grinstead and Snell's "Introduction to Probability" (2006)
#' 
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
  dat <- matrix(nrow = n_trials, ncol = length(n_rolls))
  # Run experiments
  for (i in 1:n_trials){
    dat[i,] <- sapply(n_rolls, triple_six)
  }
  # Get proportion with triple sixes at each n_rolls level
  mc_prop <- colMeans(dat) %>%
    enframe()
  colnames(mc_prop) <- c('n_rolls', 'prop')
  # Add confidence intervals for Monte Carlo Error
  mc_lower_bound <- mc_prop - 1.96*sqrt((mc_prop)*(1-mc_prop))/n_trials
  mc_upper_bound <- mc_prop + 1.96*sqrt((mc_prop)*(1-mc_prop))/n_trials
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
mc_triple_six_par <- function(n_trials, n_rolls, cl, triple_six){
  # Initialize variables
  ref_rolls <- min(n_rolls)
  iter_n <- 1:n_trials
  # Set up parallel processing
  registerDoParallel(cl)
  dat <- foreach(trial = iter_n) %dopar% {
    sapply(n_rolls, triple_six)
  }
  # unlist to matrix
  dat <- matrix(unlist(dat), ncol = length(n_rolls), byrow = TRUE)
  # Get proportion with triple sixes at each n_rolls level
  mc_prop <- colMeans(dat) %>%
    enframe()
  colnames(mc_prop) <- c('n_rolls', 'prop')
  # Add confidence intervals for Monte Carlo Error
  mc_prop <- mc_prop %>%
    mutate(
      n_rolls = n_rolls + min(ref_rolls) - 1,
      mc_lower_bound = prop - 1.96*sqrt((prop)*(1-prop))/n_trials,
      mc_upper_bound = prop + 1.96*sqrt((prop)*(1-prop))/n_trials
    )
  # Plot proportion with triple sixes at each n_rolls level against 0.5
  mc_prop %>%
    ggplot(aes(x = n_rolls, y = prop)) +
    geom_point() +
    geom_errorbar(aes(ymin = mc_lower_bound, ymax = mc_upper_bound), width = 0) +
    geom_hline(yintercept = 0.5, color = 'red') +
    scale_x_continuous(breaks = pretty_breaks()) +
    theme_minimal()
}

# My simulations take a while to run. Using autobenchmark to compare
time_compare <- microbenchmark::microbenchmark(
  'matrix' = {
    mc_triple_six(100, c(149:155))
  },
  'par' = {
    mc_triple_six_par(100, c(149:155), cl, triple_six)
  }
)
autoplot(time_compare)

# Need to figure out another way to speed this up

# try different values of trials
cl <- makeCluster(6)

tic()
test10 <- mc_triple_six_par(10000, c(148:155), cl, triple_six)
toc()

tic()
test100 <- mc_triple_six_par(100000, c(148:155), cl, triple_six)
toc()

tic()
test1M <- mc_triple_six_par(1000000, c(148:155), cl, triple_six)
toc()

# took 38.33 minutes
# 2299.535 seconds
# about 30 of those minutes were paralellized. The other 8 is the following comps
# so 150 it is

# Save plots
test10 %>%
  ggsave(paste(lubridate::today(), '_triple_six_plot_10k.jpg', sep = ''),
         .,
         path = '~/Documents/ilellosmith/r_files/Grinstead_Snell_Probability/plots',
         width = 5,
         height = 5,
         units = 'in'
  )

test100 %>%
  ggsave(paste(lubridate::today(), '_triple_six_plot_100k.jpg', sep = ''),
         .,
         path = '~/Documents/ilellosmith/r_files/Grinstead_Snell_Probability/plots',
         width = 5,
         height = 5,
         units = 'in'
  )

test1M %>%
  ggsave(paste(lubridate::today(), '_triple_six_plot_1M.jpg', sep = ''),
         .,
         path = '~/Documents/ilellosmith/r_files/Grinstead_Snell_Probability/plots',
         width = 5,
         height = 5,
         units = 'in'
  )
# Exercise 6 ----
#' Simulates n_bets in a roulette wheel
#' and calculates winnings at the end of n_bets, 
#' and returns the outcomes for each bet
#' 
#' @param n_bets - number of bets
#' @param pr_win - probability you win on a given bet
#' @param win - amount you win if you win
#' @param loss - amount you lose if you lose
#' 
#' @return list with two items: 
#'    > outcomes - tibble of outcomes and cumulative earnings (or losses)
#'    > winnings - total winnings at the end of n_bets
#'    
simulate_roulette <- function(n_bets = 1000, 
                              pr_win = 18/(18+18+2),
                              win = 1, loss = -1){
  # simulate n_bets outcomes
  outcomes <- tibble(
    'indiv_rounds' = ifelse(rbinom(n_bets, 1, pr_win), win, loss)
    ) %>%
  mutate(winnings = cumsum(indiv_rounds)) %>%
  mutate(bet_number = row_number())
  list('outcomes' = outcomes, 'total_winnings' = sum(outcomes$indiv_rounds))
}

#' Repeates roulette spin and returns dataframe
#' with labeled trials and aggregates for 
#' each trial
#' 
#' @param n_sims - number of simulations to run
#' @param n_bets - number of bets in each simulation
#' @param agg_fcn - function to aggregate cumsums
#' @param pr_win - probability of winning a given bet
#' @param win - amount of money from a win
#' @param loss - amount of money from a loss
#' 
#' @return list with two items: 
#'    > outcomes - tibble of outcomes and cumulative earnings (or losses)
#'    > winnings - total winnings at the end of n_bets
#'    
mc_roulette <- function(n_sims, n_bets,
                              pr_win, win, loss,
                              agg_fcn){
# initialize output
outcomes <- tibble(
  'indiv_rounds' = 0,
  'winnings' = 0, 
  'sim' = 0, 
  'bet_number' = 0
)
winnings <- vector(mode = 'numeric', length = n_sims)
# run n_sims
 for (i in 1:n_sims){
   # simulate a game
   game <- simulate_roulette(n_bets, pr_win, win, loss)
   # record game results
   game_outcome <- game$outcomes %>%
     mutate(sim = i)
   outcomes <- rbind(outcomes, game_outcome)
   winnings[i] <- game$total_winnings
 }
# remove seed row
outcomes <- outcomes %>%
  filter(sim > 0)
# export results
list(mc_outcomes = outcomes, agg_winnings = agg_fcn(winnings))
}

#' Plots the outcomes of many games of roulette
#' 
#' @param mc_outcomes - outcome data from mc_roulette_color
#' @param n_bets - number of bets in a given round of Roulette
#' @param pr_win - probability of winning a given bet
#' @param win - amount of money from a win
#' @param loss - amount of money from a loss
#' 
#' @return ggplot of cumulative net winnings for many rounds of 
#' roulette
#'    
plot_mc_roulette_outcomes <- function(mc_outcomes, n_bets, 
                                      pr_win, win, loss){
  # Aggregate data
  aggregate_outcome <- mc_outcomes %>%
    group_by(bet_number) %>% 
    summarize(median_winnings = median(winnings), 
              average_winnings = mean(winnings))
  # plot individual simulations and aggregate outcomes
  ggplot() + 
    geom_line(dat = mc_outcomes, 
              aes(x = bet_number, 
                  y = winnings, 
                  group = sim),
              color = 'light grey') + 
    geom_line(dat = aggregate_outcome, 
              aes(x = bet_number, 
                  y = average_winnings), 
              color = 'red') + 
    geom_segment(aes(x = 0, xend = n_bets, y = 0, yend = 0 ), color = 'black') +
    theme_minimal() +
    scale_y_continuous(labels = dollar_format(negative_parens = T)) +
    xlab('\n Bet Number') +
    ylab('Cumulative Net Winnings \n') +
    ggtitle(sprintf('Average Cumulative Net Winnings for %d Rounds of Roulette', n_bets), 
            subtitle = sprintf('Probability of winning = %.3f, win = +$%d, loss = ($%d)', pr_win, win, abs(loss)))
}

# Exercise 7 ---- 
# Betting on a red slot 
sim_red <- mc_roulette(n_sims = 300, 
                         agg_fcn = mean, 
                         n_bets = 500, 
                         pr_win = 18/(18*2+2), 
                         win = 1, 
                         loss = -1)
plot_mc_roulette_outcomes(sim_red$mc_outcomes, 
                          n_bets = 500, 
                          pr_win = 0.474,
                          win = 1,
                          loss = -1)

# Betting on a number 
sim_num <- mc_roulette(n_sims = 300, 
                   agg_fcn = mean, 
                   n_bets = 500, 
                   pr_win = 1/38, 
                   win = 36, 
                   loss = -1)
plot_mc_roulette_outcomes(sim_num$mc_outcomes, 
                          n_bets = 500, 
                          pr_win = 1/38,
                          win = 36,
                          loss = -1)

# Exercise 8 ----
# Two flips outcomes: HH, HT , TH, TT 
flip_two <- permutations(c('H', 'T'), 2, replace = T) %>% 
  # Set names
  as_tibble(.name_repair = 'minimal') %>%
  set_names('flip1', 'flip2') %>%
  # Calculate if H always in lead and total is net zero
  mutate(outcome = paste(flip1,flip2, sep = ''),
         flip1 = ifelse(flip1 == 'H', 1, -1),
         flip2 = ifelse(flip2 == 'H', 1, -1),
         always_in_lead = ifelse(flip1+flip2 == 2, 1, 0),
         net_zero = ifelse(flip1+flip2 == 0, 1,0)
  )
# Calculate proportions
prop_two <- flip_two %>% 
  summarize(prop_always_in_lead = mean(always_in_lead), 
            prop_net_zero = mean(net_zero)) %>%
  mutate(n_flip = 2)

# N times H in lead - 2 / 4
# N times H score ends at zero - 2 / 4
# Four flips outcomes: 
# Generate all outcomes
flip_four <- permutations(c('H', 'T'), 4, replace = T) %>% 
  # Set names
  as_tibble(.name_repair = 'minimal') %>%
  set_names('flip1', 'flip2', 'flip3', 'flip4') %>%
  # Calculate if H always in lead and total is net zero
  mutate(outcome = paste(flip1,flip2,flip3,flip4, sep = ''),
         flip1 = ifelse(flip1 == 'H', 1, -1),
         flip2 = ifelse(flip2 == 'H', 1, -1),
         flip3 = ifelse(flip3 == 'H', 1, -1), 
         flip4 = ifelse(flip4 == 'H', 1, -1), 
         always_in_lead = ifelse(flip1+flip2+flip3+flip4 == 4, 1, 0),
         net_zero = ifelse(flip1+flip2+flip3+flip4 == 0, 1,0)
         )
# Calculate proportions
prop_four <- flip_four %>% 
  summarize(prop_always_in_lead = mean(always_in_lead), 
            prop_net_zero = mean(net_zero)) %>%
  mutate(n_flip = 4)

# Eight flips outcomes: 
flip_eight <- permutations(c('H', 'T'), 8, replace = T) %>% 
  # Set names
  as_tibble(.name_repair = 'minimal') %>%
  set_names('flip1', 'flip2', 'flip3', 'flip4', 
            'flip5', 'flip6', 'flip7', 'flip8') %>%
  # Calculate if H always in lead and total is net zero
  mutate(outcome = paste(flip1,flip2,flip3,flip4,
                         flip5,flip6,flip7,flip8,sep = ''),
         flip1 = ifelse(flip1 == 'H', 1, -1),
         flip2 = ifelse(flip2 == 'H', 1, -1),
         flip3 = ifelse(flip3 == 'H', 1, -1), 
         flip4 = ifelse(flip4 == 'H', 1, -1), 
         flip5 = ifelse(flip5 == 'H', 1, -1),
         flip6 = ifelse(flip6 == 'H', 1, -1),
         flip7 = ifelse(flip7 == 'H', 1, -1), 
         flip8 = ifelse(flip8 == 'H', 1, -1), 
         always_in_lead = ifelse(flip1+flip2+flip3+flip4+
                                flip5+flip6+flip7+flip8 == 8, 1, 0),
         net_zero = ifelse(flip1+flip2+flip3+flip4+
                             flip5+flip6+flip7+flip8 == 0, 1,0)
  )
# Calculate proportions
prop_eight <- flip_eight %>% 
  summarize(prop_always_in_lead = mean(always_in_lead), 
            prop_net_zero = mean(net_zero)) %>% 
  mutate(n_flip = 8)

flips <- rbind(
  prop_two,
  prop_four,
  prop_eight
)

flips %>%
  ggplot() + 
  geom_line(aes(x = n_flip, y = prop_always_in_lead, color = 'Prop perm always in lead')) +
  geom_line(aes(x = n_flip, y = prop_net_zero, color = 'Prop perm net zero outcome')) + 
  scale_color_manual(values = c('Prop perm always in lead' = 'red', 
                     'Prop perm net zero outcome' = 'blue')
                     ) +
  theme_minimal() +
  theme(axis.title.y = element_blank(), 
        legend.position = 'top', 
        legend.title = element_blank()) +
  xlab('\n Number of flips per game')

# Exercise 9 ---- 
#' Plays games of roulette using the Labouchere system
#' and a starting list of bets
#' 
#' @param start_list a starting list of bets
#' @return the sum of results,  and number of roulettes to get there
#' 
play_labouchere <- function(start_list){
  # Define start list for Labouchere system
  game_results <- c()
  i = 1
  while(length(start_list) > 0){
    # Determine bet 
    bet <- 
      if(length(start_list) == 1){
        start_list
      } else {
        start_list[1] + start_list[length(start_list)] 
      }
    # Play roulette
    game_result <- simulate_roulette(n_bets = 1, pr_win = 1/2 , win = bet, loss = -bet)
    game_results[i] <- game_result$total_winnings
    i = i + 1
    # If you win, remove the first and last element of start list
    if(game_result$total_winnings > 0){
      start_list <- start_list[-c(1, length(start_list))]
    } else {
      # Otherwise, add the last bet to the end of the start list
      start_list[length(start_list)+1] <- bet
    }
  }
  list('winnings' = game_results %>% sum(),
       'plays' = length(game_results),
       'raw_bets' = game_results)
}

#' Plays many games of roulette using the Labouchere system
#' and a starting list of bets
#' 
#' @param n_reps number of times to repeat in mc experiment
#' @param start_list a starting list of bets
#' @return a list of results
#' 
mc_labouchere <- function(n_reps = 100, start_list){
  outcome <- c()
  plays <- c()
  results <- list()
 for (i in 1:n_reps){
   game <- play_labouchere(start_list)
   outcome[i] <- game$winnings
   plays[i] <- game$plays
   results[[i]] <- game$raw_bets
 } 
 list('unique_outcomes' = unique(outcome),
      'n_plays' = plays,
      'raw_bets' = results)
}

# Plot results to show why this isn't a foolproof betting strategy
my_games <- mc_labouchere(start_list= c(1,2,3,4))

# Pull out raw bets, label with game round 
bets <- my_games$raw_bets %>% 
  map(~as_tibble(.)) %>% 
  bind_rows(.id="index") %>%
  set_names(c('game', 'bet_amount')) %>%
  group_by(game) %>%
  mutate(earnings = cumsum(bet_amount),
         bet_number = row_number()) 

# Plot cumsum of earnings to illustrate why this isn't a sure betting strategy
bets %>%
  ggplot(aes(x = bet_number, y = earnings, group = game)) +
  geom_line() + 
  scale_y_continuous(labels = dollar_format(negative_parens = T)) +
  xlab('\n Bet Number') +
  ylab('Earnings \n') +
  theme_minimal()

# Plot number of plays needed to arrive at $10 winnings
my_games$n_plays %>%
  as_tibble(.) %>% 
  set_names('n_bets') %>% 
  ggplot() + 
  geom_histogram(aes(x = n_bets), bins = 40) + 
  xlab('\n Number of Bets to Get to $10') +
  ylab('') + 
  ggtitle('Density of bets required to arrive at $10 Winnings') +
  theme_minimal()

# Exercise 12 ----
# Dems have 52% support
# n-size = 1000
ifelse(rbinom(100,1000,0.52) > 500, 1,0) %>%
  table() %>% 
  prop.table()
# Dems have 51% support
# n-size = 1000
ifelse(rbinom(100,1000,0.51) > 500, 1,0) %>%
  table() %>% 
  prop.table()
# n-size = 3000
ifelse(rbinom(100,3000,0.51) > 500, 1,0) %>%
  table() %>% 
  prop.table()


# Exercise 17 ----
# a) 1D random walk
#' Simulates one random block walked 
#' 
#' @param position current position relative to start
#' @param blocks number of blocks walked 
#' 
#' @return list with position and blocks 
#'
random_block <- function(position, blocks){
  # Decide if will go left (-1) or right(1)
  position = ifelse(rbinom(1,1,0.5), position + 1, position - 1)
  # Walk one block in that direction
  blocks = blocks + 1
  # Return position and blocks
  list('position' = position, 'blocks' = blocks)
}

#' Simulates one random walk in one dimension (start to finish)
#' 
#' @return list with final position, blocks walked and record of positions
#'
random_walk_1d <- function(){
  # create empty vector for where someone has walked
  where_walked = c(0) # where a person has walked
  # Set first walk
  status <- random_block(0, 0)
  where_walked[2] <- status$position
  # Repeat until returned to start, recording number of blocks
  # travelled
  while (status$position != 0){
    status <- random_block(status$position, status$blocks)
    where_walked[status$blocks + 1] <- status$position
  }
  # return final position and blocks walked
  list('position' = status$position, 'blocks' = status$blocks, 'path' = where_walked)
}

# Simulate 100 walks
walks <- map(c(1:1000),function(x) random_walk_1d())
# Extract paths
paths <- 
  # Extract path element from walks
  sapply(walks, with, path) %>%
  # Convert to tibbles and combine
  map(~as_tibble(.)) %>% 
  bind_rows(.id = 'index') %>%
  # Name columns and record block number
  set_names(c('walk', 'position')) %>%
  group_by(walk) %>%
  mutate(block_number = row_number()) 

# Plot the random walks
paths %>%
  ggplot(aes(x = block_number, y = position, group = walk)) + 
  geom_line() + 
  theme_minimal()

# Plot the number of blocks to walk to return to zero
sapply(walks, with, blocks) %>%
  as_tibble() %>%
  set_names('blocks_walked') %>%
  filter(blocks_walked < 100) %>%
  ggplot() + 
  geom_histogram(aes(x = blocks_walked), bins = 300) + 
  xlab('\n Number of Blocks Walked') +
  ylab('') + 
  ggtitle('Density of Blocks Walked to Return to Start') +
  theme_minimal()
 
# b) 2D random walk
#' Simulates one random 2d block walked 
#' 
#' @param position current position relative to start
#' @param blocks number of blocks walked 
#' 
#' @return list with position and blocks 
#'
random_block_2d <- function(position, blocks){
  # Decide if will go up, down, left or right
  up <- c(0,1)
  down <- c(0,-1)
  left <- c(-1,0)
  right <- c(0,-1)
  direction <- sample(list(up, down, left, right), 1, replace = T)
  # Walk one block in chosen direction
  position <- position + direction[[1]]
  blocks = blocks + 1
  # Return position and blocks
  list('position' = position, 'blocks' = blocks)
}
  
