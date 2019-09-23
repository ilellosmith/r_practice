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
# preview data
str(dat)
# use all other variables to imput hd
#   > 4-6 iterations is generlly pretty good
#   > OOB is out of bag error. If does not get smaller, these estimates 
#     are about as good as we'll get
imp <- dat %>% rfImpute(hd ~., data =., iter = 6)
# run random forest and return proximity matrix 
model <- imp %>% randomForest(hd~., data =., proximity = T)
# view output of model 
#     > three types of RFs: 
#       - classification (this case, sort ppl into categories), 
#       - regression - predict a continuous variable
#       - unsupervised - happens if you don't specify a formula
#     > number of variables tried at each split:
#       - number of vars (aka columns of data tried at each node)
#       - defaults: 
#             ~ classification trees : sqrt(<number of vars>)
#             ~ regression trees: <number of vars> / 3
#             ~ but, should try to find something better than the default
model

