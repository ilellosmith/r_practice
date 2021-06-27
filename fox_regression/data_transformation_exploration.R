library(tidyverse)

set.seed(105820)

# Normal ----
# Sample from normal distribution
dat <- rnorm(10000, 0, 1) %>%
  as_tibble()

# Vizualize sample 
dat %>% 
  ggplot(aes(x = value)) + 
  geom_density()

# Positive skew ----
# Add positive skew
positive_skew <- dat %>%
  bind_rows(as_tibble(rnorm(200, 8, 1)))

# Visualize
positive_skew %>% 
  ggplot(aes(x = value)) + 
  geom_density() + 
  geom_vline(xintercept = mean(positive_skew$value))

# Negative skew ----
# Add negative skew
negative_skew <- dat %>%
  bind_rows(as_tibble(rnorm(200, -8, 1)))

# Visualize
negative_skew %>% 
  ggplot(aes(x = value)) + 
  geom_density() + 
  geom_vline(xintercept = mean(negative_skew$value))

# Transformations 
transforms <- c(function(x){1/(x+.00000001)}, # adding small amount to avoid div by 0
                function(x){log10(x+abs(min(x))+1)},  # adjusting to avoid zero
                function(x){x^2}, 
                function(x){x^3})

transformed <- map(transforms, negative_skew)
pmap(list(x = negative_skew), transforms)

transformed <- lapply(negative_skey, transforms)

transformed <- lapply(transforms, function(f) sapply(negative_skew, function(d) f(d)))
for (i in 1:length(transformed)){
  transformed[[i]] %>%
  as_tibble() %>%
  ggplot(aes(x = value)) + 
    geom_density() 
}
