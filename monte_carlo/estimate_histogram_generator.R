library(cowplot)
library(tidyverse)
library(rlang)
library(gridExtra)
library(ggeffects)


#' Return a single histogram plot of estimates around the true value
#' 
#' @param est_dat data frame: estimates
#' @param est_name string: name of estimate to be plotted 
#' @param true_value data frame: true value 
#' @param name_root string: root of column name that estimates and true value 
#' share
#' @return ggplot histogram of estimates around true value

make_plot <- function(est_dat
                      , est_name
                      , true_value
                      , name_root){
    est_dat %>%
    ggplot(aes_string(x = est_name)) +
    geom_histogram(color = 'black'
                   , alpha = 0.9) +
    # vertical line represents true value
    geom_vline(xintercept = pull(true_value)
               , color = 'red'
               , alpha = 1) +
    ylab('Frequency') +
    xlab(str_to_title(name_root)) +
    theme_cowplot()
}

#' Return a plotted grid of histograms representing the spread of 
#' estimates and true values for a series of coefficients (or other estimates) 
#' 
#' @param estimates_logit data frame of regression coefficient estimates 
#' @param true_values_logit data frame of corresponding true values for coefficients
#' @return list of histogram plots showing estimate spread around the true value  

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
  # return graphics
  graphics
}



# combine plots into a grid
combine_plots <- function(plot_list){
 plot_grid(plotlist = plot_list)
}

generate_histograms <- function(estimates_logit, true_values_logit){
  plot_list <- generate_plots(estimates_logit, true_values_logit)
  combine_plots(plot_list)
}

