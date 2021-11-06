# This script implements some code that 
# illustrates some applications of functional programming
# with purrr to data and statistics

# Load packages
library(tidyverse)
library(broom)
library(faraway)

# Set a seed for reproducibility
set.seed(42)

# Generate a random sample
x <- rnorm(35,10,6.7)


# The function my_boot_samp
# returns a single sample of data 
# with replacement for the 
# purposes of bootstrapping
my_boot_samp <- function(i){
  return(sample(x,replace=TRUE))
}

# Apply map from purrr
# to obtain 500 bootstrap resamples
# from the data x
boot_samps <- map(1:500,my_boot_samp) 

# Apply map_dbl from purrr
# to obtain 500 means from 
# the bootstrap resamples of x
boot_means <- map_dbl(boot_samps,mean)

# Obtain bootstrap percentile intervals to
# approximate a 95% confidence interval
# for the mean
quantile(boot_means,probs = c(0.025,0.975))

# Compare bootstrap approximation to
# analytical formula for confidence interval
mean(x) + 1.96*c(-1,1)*(sd(x)/sqrt(length(x)))


# The remainder of the code applies 
# bootstrapping to parameter estimates
# for a linear model
tibble(boot_means=boot_means) %>% ggplot(aes(x=boot_means)) + 
  geom_histogram()

help("leafburn")

boot_samp_leafburn <- function(i){
  boot_df <- sample_n(leafburn,nrow(leafburn),replace = TRUE)
}

leafburn_boots <- map(1:500,boot_samp_leafburn)

boot_leafburn_df <- tibble(boot_samps=leafburn_boots)

leafburn_boot_fit <- function(df){
  lm_fit <- lm(burntime~nitrogen+chlorine+potassium,data=df)
  return(tidy(lm_fit))
}

boot_leafburn_df <- boot_leafburn_df %>%
  mutate(fits=map(boot_samps,leafburn_boot_fit))

boot_leafburn_df %>% unnest(fits) %>%
  group_by(term) %>%
  ggplot(aes(x=estimate)) + geom_histogram() + 
  facet_wrap(~term)



