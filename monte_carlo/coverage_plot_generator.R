#' Return a single histogram plot of estimates around the true value
#' 
#' @param est_dat data frame: estimates
#' @param est_name string: name of estimate to be plotted 
#' @param true_value data frame: true value 
#' @param name_root string: root of column name that estimates and 
#' true value share
#' @param color_map vector: named vector that maps hex values to labels
#' @return ggplot of CI coverage

make_plot <- function(est_dat
                      , est_name
                      , true_value
                      , name_root
                      , color_map){
  est_dat %>% 
    ggplot() +
    geom_point(aes(x = x_val,
                   y = estimate, 
                   color = color)) +
    geom_linerange(aes(x = x_val,
                       ymin = lower_bound,
                       ymax = upper_bound,
                       color = color)) +
    geom_hline(yintercept = pull(true_value), color = 'red') +
    scale_color_manual(values = color_map,
                       labels = c('CI includes true value',
                                  'CI does not include true value')) +
    xlab(paste(nrow(est_dat), 'Simulated Samples', sep = ' ')) +
    ylab('coeff_x1') +
    theme_cowplot() +
    theme(legend.position = 'top',
          legend.title = element_blank()) 
  
  
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




cov_probs <- calculate_coverage(estimates_logit$coeff_x1_est
                                , estimates_logit$coeff_x1_se
                                , true_values_logit$coeff_x1_tru, level = 0.95)

combo <- cbind(cov_probs$ci
               , true_values_logit$coeff_x1_tru 
               , cov_probs$truth_in_ci
               , estimates_logit$coeff_x1_est)
colnames(combo) <- c('lower_bound', 'upper_bound'
                     , 'true_value', 'truth_in_ci', 'estimate')
combo <- combo %>% as_tibble()


color_map <- c(
  'ci_covers_truth' = '#999999'
  , 'truth_outside_ci' = '#0066CC' 
  , 'true_value_color' = '#CC0000'
)
to_plot <- combo %>% 
  sample_n(100) %>%
  mutate(x_val = row_number()) %>%
  mutate(color = ifelse(truth_in_ci == 1, 'ci_covers_truth' , 
                        'truth_outside_ci'))
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


