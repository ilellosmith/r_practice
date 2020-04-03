# This script reads in data from Littorea measurements taken at different times
# and calculates the deltas from one time to another based on user specifications 

# Functions and code are pretty well documented, with TODO's in places where things
# are particularly likely to break if you make changes to what variables you
# want to analyze, dataset structure etc.

# Work Flow:
# 1) Change Global variables section to fit input and output file paths to your machine, 
#    and, if desired, to specify a subset of variables (if unchanged, it'll analyze all variables)
# 2) Run full script and check for output file in specified output file path
# ---------------------------------------------------

# Load Packages 

# ---------------------------------------------------
library(tidyverse)

# ---------------------------------------------------

# Define Functions

# ---------------------------------------------------
#' Takes a dataframe with time series measurements and variable name,
#' returns the deltas between the time series in output dataframe
#' @param dat - a dataframe 
#' @param variable_name - string representing variable name
#' 
#' NB: this function is very brittle. By that I mean that if you 
#' use it to run across different time frames, or variables (e.g. "time_point" instead of "tp") 
#' it will break. 
calculate_unidelta <- function(dat, variable_name){
  dat %>% 
    select(tp:id, {{variable_name}}) %>%
    # Combine treatment + jar + id values to create individual id
    mutate(indiv = paste(ll_treat,jar, id)) %>% 
    select(indiv, tp, {{variable_name}}) %>%
    group_by(indiv) %>% 
    pivot_wider(names_from = tp, values_from = {{variable_name}}) %>%
    # TODO: If you change time intervals, add here 
    mutate(delta_0_30 = `30`-`0`
           , delta_30_60 = `60`-`30`
           , delta_60_90 = `90`-`60`
           , delta_0_90 = `90`-`0`
    ) %>%
    # make tidy again in two steps
    # make time long
    pivot_longer(cols = `0`:`90`
                 , names_to = 'tp'
                 , values_to = variable_name) %>%
    ungroup() %>%
    # Break out individual ids into original columns
    separate(col = indiv
             , into = c('condition', 'jr', 'id')
             , sep = ' ') %>%
    # Split variable and measurements out to columns 
    pivot_longer(cols = {{variable_name}}
                 , names_to = 'variable'
                 , values_to = 'measurement')
}

#' Iterates over variables, calculating deltas 
#' Then combines into single output dataframe
#' @param dat - a dataframe 
#' @param columns_to_calculate - string representing variable names to calculate deltas for 
calculate_all_deltas <- function(dat
                                 , columns_to_calculate){
  all_deltas <- list()
  for (variable in columns_to_calculate){
      all_deltas[[variable]] <- calculate_unidelta(dat, variable)
  }
  return(all_deltas)
}

# ---------------------------------------------------

# Define Global Variables

# ---------------------------------------------------
# TODO: Change these file paths for your machine
ORIGINAL_DATA_FILE_PATH <- '/Users/isaaclello-smith/Documents/Littorea0-90.2020_clean_tall.xlsx'
OUTPUT_DATA_FILE_PATH <- '/Users/isaaclello-smith/Documents/Littorea0-90.2020_clean_tall_deltas.xlsx'
# TODO: If you want to analyze all variables use the below definition
VARIABLES_SUBSET_TO_ANALYZE <- NULL
# TODO: If you want to analyze a subset of variables, uncomment and specify them in a character vector below
# VARIABLES_SUBSET_TO_ANALYZE <- c('variable1', 'variable2', 'variable3', 'variable4',...)

# ---------------------------------------------------

# Read and QA Data

# ---------------------------------------------------
original_dat <- readxl::read_excel(ORIGINAL_DATA_FILE_PATH)
original_dat <- original_dat %>% 
  janitor::clean_names
original_dat %>% 
  summary()

# ---------------------------------------------------

# Run functions across all variables (or subset) and export data

# ---------------------------------------------------
if(VARIABLES_SUBSET_TO_ANALYZE > 0){
# This conditional flow is for analyzing a subset of variables
# Pull columns to calculate 
columns_to_calculate <- original_dat %>% 
  # TODO: Change these variables if you want to analyze a smaller subset
  select(VARIABLES_SUBSET_TO_ANALYZE) %>%
  colnames()
} else {
 # If no subset of variables specified, analyze all of them
  # Pull columns to calculate 
  columns_to_calculate <- original_dat %>% 
    # TODO: Change these variables if you want to analyze a smaller subset
    select(immersed:ll_bw_mean_mm) %>%
    colnames()
}

# Calculate Deltas
all_deltas <- calculate_all_deltas(
  original_dat
  , columns_to_calculate
)

# Export named list to excel sheet
writexl::write_xlsx(
  x = all_deltas
  , path = OUTPUT_DATA_FILE_PATH
)