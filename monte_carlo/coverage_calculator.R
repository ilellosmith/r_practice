#' Function to calculate confidence interval coverage for glms 
#' Defined in Carsey & Harden (2014)
#' 
#' - Takes a vector of estimates, standard errors for the estimates, 
#' a true value, confidence level and dfs
#' - Calculates and returns a list of coverage probability (using Student T dist), 
#' a vector of whether or not a given CI estimate contains the true value, 
#' the bounds of the CI and monte carlo error (using SE formula)
#' 
#' @param b - Vector of estimates, e.g. B0, B1 etc. 
#' @param se - Vector of standard errors associated with estimates
#' @param truth - True parameter
#' @param level - Confidence level
#' @param df - Degrees of freedom. Default is Inf to make Tdist == Normal. While 
#' appropriate for large samples, this is not a valid assumption in models defined 
#' on smaller datasets. Set df to n-model$rank to calculate more accurate df. 
#' @return - List containing:
#' > coverage probability (numeric)
#' > whether the CI for each estimate covers the true parameter (0/1 vector)
#' > the CI bounds (dataframe)
#' > Monte Carlo error bounds (numeric vector). This describes simulation error 
#' that emerges from a finite number of repetitions. Calculated from the normal 
#' approximation to the binomial distribution (calculating bounds around a binomial proportion)
#' NB: this method is not robust to proportions very close to 0 or 1. 

calculate_coverage <- function(b, se, truth, level = 0.95, df = Inf){
  qtile <- level + (1-level)/2 # quantile for Tdist
  # Calculate confidence interval
  lower_bound <- b - qt(qtile, df = df)*se 
  upper_bound <- b + qt(qtile, df = df)*se
  # True parameter truth in CI? 
  truth_in_ci <- ifelse(truth >= lower_bound & 
                          truth <= upper_bound, 1, 0)
  # Calculate coverage probability
  cvg_pr <- mean(truth_in_ci, na.rm = T)
  # Calculate Monte Carlo Error
  mc_lower_bound <- cvg_pr - 1.96*sqrt((cvg_pr)*(1-cvg_pr))/length(b)
  mc_upper_bound <- cvg_pr + 1.96*sqrt((cvg_pr)*(1-cvg_pr))/length(b)
  # Silent return results 
  list(coverage_probability = cvg_pr
       , truth_in_ci = truth_in_ci
       , ci = cbind(lower_bound, upper_bound)
       , mc_error_bounds = c(mc_lower_bound, mc_upper_bound))
}
