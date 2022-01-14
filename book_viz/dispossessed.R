library(tidytext)
library(stringr)
dat <- readLines('~/Desktop/dispossessed.txt')
dat_as_one_string <- dat %>% 
  paste(collapse=" ")
punctuation <- str_extract_all(dat_as_one_string, '[.|,|"|”|“|?|!|—|:|;|...]')
punctuation_as_one_string <- punctuation[[1]] %>% 
  paste(collapse=" ")
punctuation_as_one_string
nchar(punctuation_as_one_string)
write.table(punctuation_as_one_string, file = "~/Desktop/dispossessed_punctuation.txt", sep = "")
my_str <- 'this is a test string that is some length'
cSplit(my_str)
