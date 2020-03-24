library(cowplot)
library(tidyverse)
library(rlang)
library(gridExtra)
library(ggeffects)

# Make a single histogram plot using estimate data
# an estimate name, a true value that is being estimated
# and the root of the estimate and true value name
make_plot <- function(est_dat
                      , est_name
                      , true_value
                      , name_root){
  plot <- est_dat %>%
    ggplot(aes_string(x =est_name )) +
    geom_histogram(color = 'black'
                   , alpha = 0.9) +
    geom_vline(xintercept = pull(true_value)
               , color = 'red'
               , alpha = 1) +
    ylab('Frequency') +
    xlab(str_to_title(name_root)) +
    theme_cowplot()
  return(plot)
}

# combine plots into a grid
combine_plots <- function(plot_list){
 plot_grid(plotlist = plot_list)
}

generate_histograms <- function(estimates_logit, true_values_logit){
  plot_list <- generate_plots(estimates_logit, true_values_logit)
  combine_plots(plot_list)
}

# take in an unspecified number of estimates and 
# plot a histogram of each as well as the true value 
# combine them on the same plot

# what is the identifying feature? 
# it's the column (estimate) name
# apply across columns with est in the name 

# takes in dataframe of estimates, returns a list of histogram graphics objects
generate_plots <- function(estimates_logit, true_values_logit) {
  # limit to estimates columns
  estimates <- estimates_logit %>% 
    select_if(str_detect(colnames(estimates_logit), '_est'))
  # get names for iteration
  names <- colnames(estimates)
  # generate plots
  graphics <- list()
  for (name in names){
    # root of the estimate name that corresponds to root of true value 
    name_root <- str_extract(name, '.+(?=_est)')
    # get true value corresponding to root
    true_value <- true_values_logit %>% select_if(str_detect(colnames(.),name_root))
    # select estimate data
    this_est <- estimates[name]
    # store plot in list
    graphics[[name_root]] <- make_plot(this_est, name, true_value, name_root)
  }
  return(graphics)
}
  
# map_if(estimates_logit, str_detect(colnames(estimates_logit), '_est'), hist)