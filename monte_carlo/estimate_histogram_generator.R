library(cowplot)
library(tidyverse)
library(rlang)
library(gridExtra)
library(ggeffects)

#' Take input string, and format to be string for a label
#' @param string string: string to format 
#' @return string: cleaned string for label
str_to_lab <- function(string){
  str_to_title(str_replace(string, "_", " "))
}


#' Return a single histogram plot of estimates around the true value
#' 
#' @param est_dat tibble: estimates
#' @param est_name string: name of estimate to be plotted 
#' @param true_value tibble: true value 
#' @param name_root string: root of column name that estimates and true value 
#' share
#' @return ggplot histogram of estimates around true value

make_hist_plot <- function(est_dat
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
    xlab(str_to_lab(name_root)) +
    theme_cowplot()
}

#' Return a plotted grid of histograms representing the spread of 
#' estimates and true values for a series of coefficients (or other estimates) 
#' 
#' @param estimates_logit tibble: logistic regression coefficient estimates 
#' @param true_values_logit tibble: true coefficient values from DGP
#' @return list of histogram plots showing estimate spread around the true value  

# takes in dataframe of estimates, returns a list of histogram graphics objects
generate_hist_plots <- function(estimates_logit, true_values_logit) {
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
    graphics[[name_root]] <- make_hist_plot(this_est, name, true_value, name_root)
  }
  # return graphics
  graphics
}


#' Generate and plot histograms
#' @param estimates_logit tibble: logistic regression coefficient estimates 
#' @param true_values_logit tibble: true coefficient values from DGP
#' @return cowplot of histogram of estimates and true values
plot_histograms <- function(estimates_logit, true_values_logit){
  plot_list <- generate_hist_plots(estimates_logit, true_values_logit)
  plot_grid(plotlist = plot_list)
}

