library(tidyverse)

# Create sample data 
projects <- tibble(
  project_name = c('Isaacs Map', 'Louies Map', 'Isaacs Fun Map Two', NA, NA,NA),
) %>% 
  mutate(
    across(everything(), str_to_lower)
    )

files <- tibble(
  file_name = c('someFilewithIsaacsMap.file', 'IsaacsfunMapFile.file',
              'anothersamplefilemapLouies.file', 'samsexampleFileMapname.file'
              , 'anotherfile_name_for_fun.file', 'and2021anextra.file')
) %>% 
  mutate(
    across(everything(), str_to_lower)
  )

# Get list of unique files
unique_files <- files %>% 
  pull() %>%
  unique() %>%
  list()

#' Returns a boolean list for whether
#' each word in project name matches 
#' in file_name
#' 
#' @param project_name a string with project name
#' @param file_name a string with file_name
search_for_project_terms <- function(project_name, file_name){
  # get words for project name
  project_words <- str_split(project_name, pattern = ' ')
  # search for each in file_name
  map(project_words, ~str_detect(file_name, .))
}

#' Returns a list of the output of search_for_project_terms 
#' for each unique file
#' 
#' @param project_name a string with project name
#' @param file_name a string with file_name
check_files <- function(project_name, file_names){
 matches <- list()
 for (file_name in file_names){
   matches[file_name] <- search_for_project_terms(project_name, file_name)
 }
 matches
}


projects %>% 
  mutate(file_matches = map(unique_files[[1]], ~check_files(project_name, .))) %>% View()

pwalk(projects, unique_files, search_for_project_terms)
