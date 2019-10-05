# In this script I:
# 1) practice running random forests following Josh Starmer's StatQuest tutorial
# 2) rewrite some of his code to be tidy, and
# 3) explore tic toc and microbenchmark packages for comparing time between 
#    different code blocks 

# ---- Set up environment ----

# load dependencies, installing if necessary
REQUIRED_PACKAGES <- c("tidyverse", 'cowplot', 'randomForest', "tidyr"
                       , "tictoc", 'microbenchmark')
package.check <- lapply(REQUIRED_PACKAGES, FUN = function(x) {
  if (! require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

set.seed(42)

# color blind pallette grey
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# color blind pallette black
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ---- Read in and clean data ----

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

# replace NAs, convert to factors
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

# ---- Impute hd and run random forest ----

# use all other variables to impute hd
#   > 4-6 iterations is generlly pretty good
#   > OOB is out of bag error. If does not get smaller, these estimates 
#     are about as good as we'll get
imp <- dat %>% rfImpute(hd ~., data =., iter = 6)
# does increasing iteration improve the estimates?
set.seed(42)

imp_20 <- dat %>% rfImpute(hd ~., data =., iter = 20)
# run random forest and return proximity matrix 
set.seed(42)

model <- imp %>% randomForest(hd~., data =., proximity = T)
# view output of model 
#     > three types of RFs: 
#       - classification - this use case, sort things into categories, 
#       - regression - predict a continuous variable
#       - unsupervised - happens if you don't specify a formula
#     > number of variables tried at each split:
#       - number of vars (aka columns of data tried at each node)
#       - defaults: 
#             ~ classification trees : sqrt(<number of vars>)
#             ~ regression trees: <number of vars> / 3
#             ~ but, should try to find something better than the default
#     > OOB estimate of error rate is percent of OOB samples that were incorrectly
#       classified by the random forest
model

# ---- Evaluate model error, is 500 enough for optimal classification? ----

# graphically determine if 500 trees enough for optimal classification
# preview error rate matrix
#   - first column is OOB error rate
#   - second column is how often healthy patients are misclassified
#   - third column is how often unhealthy patients are misclassified
# row n reflects error rates after n trees
model$err.rate
# make error rate matrix tidy
#   - first column is number of trees
#   - second column is type of error
#   - third column is error measurement
oob_error_data <- data.frame(
  Trees = rep(1:nrow(model$err.rate), times = 3), 
  Type = rep(c("OOB", "Healthy", "Unhealthy"), each = nrow(model$err.rate)),
  Error = c(model$err.rate[, "OOB"],
            model$err.rate[, "Healthy"], 
            model$err.rate[, 'Unhealthy'])
)

# implementation with tidyverse instead
oob_error_data2 <- as_tibble(model$err.rate) %>%
  rowid_to_column("Trees") %>%
  group_by(Trees) %>%
  pivot_longer(cols = c('OOB', 'Healthy', 'Unhealthy')
               , names_to = "Type"
               , values_to = 'Error') %>%
  ungroup() %>%
  arrange(Type, Trees)

# both generate the same graphic in ggplot
oob_error_data2 %>%
  ggplot(aes(x = Trees, y = Error)) +
  geom_line(aes(color = Type))

oob_error_data %>%
  ggplot(aes(x = Trees, y = Error)) +
  geom_line(aes(color = Type))

# ---- which is faster? : exploration of timing ----

# compare two methods for measuring time 
# chose tictoc and microbenchmark based on:
# https://www.r-bloggers.com/5-ways-to-measure-running-time-of-r-code/

# tic / toc
tic.clearlog()
# manual construction 
tic('manual construction')
oob_error_data <- data.frame(
  Trees = rep(1:nrow(model$err.rate), times = 3), 
  Type = rep(c("OOB", "Healthy", "Unhealthy"), each = nrow(model$err.rate)),
  Error = c(model$err.rate[, "OOB"],
            model$err.rate[, "Healthy"], 
            model$err.rate[, 'Unhealthy'])
)
toc(log = T)

# tidyverse
tic("tidyverse implementation")
oob_error_data2 <- as_tibble(model$err.rate) %>%
  rowid_to_column("Trees") %>%
  group_by(Trees) %>%
  pivot_longer(cols = c('OOB', 'Healthy', 'Unhealthy')
               , names_to = "Type"
               , values_to = 'Error') %>%
  ungroup() %>%
  arrange(Type, Trees)
toc(log = T)

# tic.log()
man <- parse_number(tic.log()[[1]])
tdy <- parse_number(tic.log()[[2]])
print(paste('the tidy implementation takes ', tdy/man , 
            'x as long as the manual implementation', sep = ''))

# love the simplicity of tic/toc, but
# it's annoying to have to parse for the numbers

# microbenchmark

mbm <- microbenchmark("manual" = {
  oob_error_data <- data.frame(
    Trees = rep(1:nrow(model$err.rate), times = 3), 
    Type = rep(c("OOB", "Healthy", "Unhealthy"), each = nrow(model$err.rate)),
    Error = c(model$err.rate[, "OOB"],
              model$err.rate[, "Healthy"], 
              model$err.rate[, 'Unhealthy'])
  )
}
, "tidy" = {
  oob_error_data2 <- as_tibble(model$err.rate) %>%
    rowid_to_column("Trees") %>%
    group_by(Trees) %>%
    pivot_longer(cols = c('OOB', 'Healthy', 'Unhealthy')
                 , names_to = "Type"
                 , values_to = 'Error') %>%
    ungroup() %>%
    arrange(Type, Trees)
}
)

autoplot(mbm)
mbm

# I. love. this. package.
#    - the autoplot is so easy 
#    - the output object has really detailed metrics, like best case, average and worst case run time
#    - clearly runs on some iterative testing to derive these numbers. see mbm$time and mbm$expr
#    - wouldn't use this on really time intensive code

# ---- Try running random forest with 1000 iterations instead of 500 to compare timing and model accuracy ---- 

# this takes a good while, though it's nothing too crazy
# really a tradeoff of efficiency and information
set.seed(123456)
rf_compare <- microbenchmark(
"1000 trees" = {model_1000 <- imp %>% randomForest(hd~., data =., ntree= 1000, proximity = T)}
, "500 trees" = {model_500 <- imp %>% randomForest(hd~., data =., ntree = 500, proximity = T)}
)

autoplot(rf_compare)
# what if we want to go beyond the default plot?
rf_compare_dat <- rf_compare %>% 
  as_tibble()
# boxplot
rf_compare_dat %>% 
  group_by(expr) %>%
  ggplot(aes(x = expr, y = time)) +
  geom_boxplot() +
  scale_y_log10()
# compare summary stastics
stats <- rf_compare_dat %>% 
  group_by(expr) %>% 
  summarize(mean = mean(time)
            , median = median(time)
            , sd = sd(time)
            , min = min(time)
            , max = max(time)
  )
print('the following summary stats measure the 1000 trees run time / 500 trees run time:')
(stats %>% 
  filter(expr == '1000 trees') %>%
  select(-expr)
  ) / (stats %>% 
  filter(expr == '500 trees') %>%
  select(-expr)
  )
# in this case it seems to scale linearly, the metrics just about double
# across the board

model_500
model_1000

# OOB error rate did improve by ~ .33%, correctly classifying
# two additional healthy patients but misclassifying an unhealthy one
# What does our error rate to N trees plot look like with 1000 reps?

error_rate_1000 <- as_tibble(model_1000$err.rate) %>%
  rowid_to_column("Trees") %>%
  group_by(Trees) %>%
  pivot_longer(cols = c('OOB', 'Healthy', 'Unhealthy')
               , names_to = "Type"
               , values_to = 'Error') %>%
  ungroup() %>%
  arrange(Type, Trees)

error_rate_1000 %>%
  ggplot(aes(x = Trees, y = Error)) +
  geom_line(aes(color = Type))

# what is the optimal number of variables at each internal node in the tree?
set.seed(123456)
oob_values <- vector(length=10)
for (i in 1:10) {
  temp_model <- randomForest(hd ~ ., data = imp, mtry = i, ntree = 1000)
  oob_values[i] <- temp_model$err.rate[nrow(temp_model$err.rate),1]
}
which.min(oob_values)
# it appears the optimal number of variables is 4
set.seed(123456)
model_optimal_vars <- randomForest(hd ~ .
                           , data = imp
                           , mtry = which.min(oob_values)
                           , ntree = 1000
                           , proximity = T)
error_rate_optimal_vars <-
  as_tibble(model_optimal_vars$err.rate) %>%
  rowid_to_column("Trees") %>%
  group_by(Trees) %>%
  pivot_longer(cols = c('OOB', 'Healthy', 'Unhealthy')
               , names_to = "Type"
               , values_to = 'Error') %>%
  ungroup() %>%
  arrange(Type, Trees) %>%
  mutate(n_vars =  which.min(oob_values))

error_rate_1000_vars <- 
  error_rate_1000 %>%
  mutate(n_vars = 3)

error_rate_optimal_vars %>%
  mutate(n_vars = which.min(oob_values)) %>%
  merge(error_rate_1000, by = 'Trees', all = T) 

compare <- bind_rows(error_rate_1000_vars,
                     error_rate_optimal_vars) %>%
  arrange(Type, Trees, n_vars)

# plotting with 3 vs. 4 vars
# using the mtry identified with our above iteration, OOB with 4 is better than
# OOB with 3, as is Unhealthy, though Healthy seems to be the same

compare <- compare %>%
  mutate(color = fct_relevel(interaction(Type, n_vars), c('Unhealthy.3'
                                                          , 'Unhealthy.4'
                                                          , 'OOB.3'
                                                          , 'OOB.4'
                                                          , 'Healthy.3'
                                                          , 'Healthy.4'
                                                          )))

ggplot() +
  geom_line(data = compare
            , aes(x = Trees, y = Error, color = reorder(color, Type))) +
  scale_color_manual(values = cbPalette) 

# exact differences
compare %>%
  filter(Trees == 1000)

# interestingly, on average and on median in the 1000 > Trees > 250 
# three vars does better than 4 for OOB and Healthy
# it's also slightly more reliable w.r.t. sd 
compare %>%
  group_by(Type, n_vars) %>%
  filter(Trees > 250) %>%
  summarize(mean_error = mean(Error)
            , median_error = median(Error)
            , sd_error = sd(Error))

# MDS plot ----

distance_matrix <- dist(1-model_optimal_vars$proximity)
mds_scaled <- cmdscale(distance_matrix, eig= T, x.ret= T)
mds_percentage_variation_explained <- round(mds_scaled$eig/sum(mds_scaled$eig)*100,1)
mds_values <- mds_scaled$points  
mds_data <- tibble(Sample = rownames(mds_values),
                       X = mds_values[,1],
                       Y = mds_values[,2],
                       Status = imp$hd)
mds_data %>% 
  ggplot(aes(x = X, y = Y, label = Sample)) +
  geom_text(aes(color = Status)) +
  theme_bw() + 
  xlab(paste('MDS1 - ', mds_percentage_variation_explained[1], '%', sep = "")) +
  ylab(paste('MDS2 - ', mds_percentage_variation_explained[2], '%', sep = "")) +
  ggtitle('MDS plot using (1 - Random Forest Proximities)')

