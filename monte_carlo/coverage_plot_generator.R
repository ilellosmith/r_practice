#' Returns a single plot of estimates and CIs around the true value
#' NB: For ease of interpretation, this function should plot an input sample
#' of estimates. 
#' 
#' @param est_ci_dat tibble: confidence intervals and estimates
#' @param est_name string: name of estimate to be plotted 
#' @param true_value tibble: true value for estimates
#' @param name_root string: root of column name that estimates and 
#' true value share
#' @param color_map vector: named vector that maps hex values to labels
#' @return ggplot object of CI coverage

make_ci_plot <- function(est_ci_dat
                      , true_value
                      , name_root
                      , color_map){
  est_ci_dat %>% 
    ggplot() +
    geom_point(aes(x = x_index,
                   y = estimate, 
                   color = color)) +
    # Confidence Intervals
    geom_linerange(aes(x = x_index,
                       ymin = lower_bound,
                       ymax = upper_bound,
                       color = color)) +
    # True value
    geom_hline(yintercept = true_value, color = 'red') +
    scale_color_manual(values = color_map,
                       labels = c('CI includes true value',
                                  'CI does not include true value')) +
    xlab(paste(nrow(est_ci_dat), 'Simulated Samples', sep = ' ')) +
    ylab(str_to_lab(name_root)) +
    theme_cowplot() +
    theme(legend.position = 'top',
          legend.title = element_blank()) 
  
}


#' Returns a sample of estimates and CIs around the true value for a given
#' coefficient
#' 
#' @param coeff_est numeric vector: estimates, e.g. one of: B0, B1 etc. 
#' @param coeff_se numeric vector: standard errors associated with estimates
#' @param true_value numeric: true value for estimates
#' @param level numeric: Confidence level
#' @param df numeric: Degrees of freedom. Default is Inf to make Tdist == Normal,
#' valid for large samples. See calculate_coverage definition for explanation and
#' small sample alternative
#' @param n_ci numeric: number of CIs to include in a plot
#' @return tibble: sample of n_ci CIs for plotting
get_ci_coverage_dat <- function(# CI coverage values
                                coeff_est
                                , coeff_se
                                , true_value
                                , level = 0.95
                                , df = Inf
                                # Plot Parameters
                                , n_ci
) {

# calculate confidence interval coverage
cover_probs <- calculate_coverage(coeff_est, coeff_se, true_value, level, df)
  
  # estimates_logit$coeff_x1_est
  # , estimates_logit$coeff_x1_se
  # , true_values_logit$coeff_x1_tru, level = 0.95)

# combine data for plot
cover_plot_dat <- cbind(coeff_est, cover_probs$ci, true_value
                        , cover_probs$truth_in_ci)

  # estimates_logit$coeff_x1_est
  # , true_values_logit$coeff_x1_tru 
  # , cover_probs$truth_in_ci

# set column names
colnames(cover_plot_dat) <- c('estimate', 'lower_bound', 'upper_bound'
                              , 'true_value', 'truth_in_ci')

# make tibble
cover_plot_dat <- cover_plot_dat %>% as_tibble()

# get CI sample for plotting 
# set target coverage probability
true_coverage <- round(cover_probs$coverage_probability, digits = 2)
# pull sample
get_ci_sample(cover_plot_dat, true_coverage, n_ci)

}

#' Takes a potentially large set of confidence intervals and estimates and 
#' repeatedly samples a specified number of CIs to plot until the sample reflects the 
#' true coverage probability
#' 
#' @param est_ci_dat tibble: confidence intervals and estimates
#' @param true_coverage numeric: true coverage probability
#' @param n_ci numeric: number of CIs to return
#' 
#' @return a set of CIs to plot with CI coverage labels

get_ci_sample <- function(est_ci_dat
                          , true_coverage
                          , n_ci = 100){
  # write a formula to check when the while loop fails
  # to converge. 
  
  # alternatively, change so it times out 
  
  # initialize coverage value
  plot_coverage = Inf
  
  # Resample until rounded plot_coverage = rounded true coverage
  while(plot_coverage != true_coverage){
    
    # sample n_ci estimates + CIs    
    to_plot <- combo %>% 
      sample_n(n_ci) 
    
    # get the proportion of CIs in sample that cover true estimate
    plot_coverage <- to_plot %>%
      summarize(mean(truth_in_ci)) %>%
      pull() %>%
      round(digits = 2)
    
  }
  
  # export final sample with x index and ci coverage label
  to_plot %>%
    mutate(x_index = row_number()) %>%
    mutate(color = ifelse(truth_in_ci == 1, 'ci_covers_truth' , 
                          'truth_outside_ci')) 
}



color_map <- c(
  'ci_covers_truth' = '#999999'
  , 'truth_outside_ci' = '#0066CC' 
  , 'true_value_color' = '#CC0000'
)


generate_ci_plots <- function(color_map, n_ci){
  
  
}

#' Generate and plot confidence interval coverage plots

#' @return cowplot of confidence interval coverage plots 
plot_ci_coverage <- function(){
  plot_list <- generate_ci_plots()
  plot_grid(plotlist = plot_list)
}



true_value <- to_plot %>%
  distinct(true_value) %>%
  pull()
true_value <- true_values_logit$coeff_x1_tru

to_plot %>% 
  ggplot() +
  geom_point(aes(x = x_val,
                 y = estimate, 
                 color = color)) +
  geom_linerange(aes(x = x_val,
                     ymin = lower_bound,
                     ymax = upper_bound,
                     color = color)) +
  geom_hline(yintercept = true_value, color = color_map['true_value_color']) +
  scale_color_manual(values = color_map,
                     labels = c('CI includes true value',
                                'CI does not include true value')) +
  xlab('100 Simulated Samples') +
  ylab('coeff_x1') +
  theme_cowplot() +
  theme(legend.position = 'top',
        legend.title = element_blank()) 


