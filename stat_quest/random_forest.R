# In this script I practice running random forests following Josh Starmer's
# StatQuest tutorial

# load dependencies, installing if necessary
REQUIRED_PACKAGES <- c("tidyverse", 'cowplot', 'randomForest')
package.check <- lapply(REQUIRED_PACKAGES, FUN = function(x) {
  if (! require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

set.seed(42)

# read in and clean data
page_url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/'
data_set <- 'processed.cleveland.data'
dat <- read_csv(paste(page_url, data_set, sep = ''),
                col_names = F)
colnames(dat) <- c(
  'age'
  , 'sex'
  , 'cp'
  , 'trestbps'
  , 'chol'
  , 'fbs'
  , 'restecg'
  , 'thalach'
  , 'exang'
  , 'oldpeak'
  , 'slope'
  , 'ca'
  , 'thal'
  , 'hd'
)
# replace NAs
dat <- dat %>% 
  mutate_if(is.character, 
                  str_replace_all,
                  pattern = "\\?",
                  replacement = NA_character_) %>%
  mutate(sex = as.factor(ifelse(sex, 'M', 'F')), 
    hd = as.factor(ifelse(hd, 'Unhealthy', 'Healthy'))) %>%
  mutate(ca = as.factor(as.integer(ca)), 
         thal = as.factor(as.integer(thal))) %>%
  mutate_at(vars(cp
            , fbs
            , restecg
            , exang
            , slope) 
            , as.factor)
str(dat)
