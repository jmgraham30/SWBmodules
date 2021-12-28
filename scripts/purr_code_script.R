# This script implements some code that 
# illustrates some applications of functional programming
# with purrr to data and statistics

# Load packages
library(tidyverse)
library(broom)
library(faraway)

# Set a seed for reproducibility
set.seed(42)


# Create a data frame of simulated data

sim_data <- map(1:5,~rnorm(15,6,1.8)) # creates a list

sim_df <- map_dfc(1:5,~rnorm(15,6,1.8)) # creates a data frame

sim_df <- janitor::clean_names(sim_df)

# Standardize Each Column

my_stdzr <- function(x){
  return((x-mean(x))/sd(x))
}

sim_df_std <- map_df(sim_df,my_stdzr)

# Apply t-test to Each Column

my_t_test <- function(x){
  res <- t.test(x,mu=6)
  return(res$p.value)
}

test_results <- map_dbl(sim_df,my_t_test)

# Apply t-tests with varying means

my_t_test_multi <- function(x,mu_val){
  res <- t.test(x,mu=mu_val)
  return(res$p.value)
}

multi_test_results <- map2(sim_df,c(6,7,9,6,2),my_t_test_multi)

# or

multi_test_results_dbl <- map2_dbl(sim_df,c(6,7,9,6,2),my_t_test_multi)

### More Sophisticated Examples: Bootstrapping

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


# Plot of bootstrap means
tibble(boot_means=boot_means) %>% ggplot(aes(x=boot_means)) + 
  geom_histogram()

# The remainder of the code applies 
# bootstrapping to parameter estimates
# for a linear model

# We use the leafburn data set from the faraway package
help("leafburn")

# A function to resample data
boot_samp_leafburn <- function(i){
  boot_df <- sample_n(leafburn,nrow(leafburn),replace = TRUE)
}

# Obtain bootstrap samples
leafburn_boots <- map(1:500,boot_samp_leafburn)

# Add as a list column to a data frame
boot_leafburn_df <- tibble(boot_samps=leafburn_boots)

# A function to fit bootstrap samples
leafburn_boot_fit <- function(df){
  lm_fit <- lm(burntime~nitrogen+chlorine+potassium,data=df)
  return(tidy(lm_fit))
}

# Bootstrap parameters from linear model fit
boot_leafburn_df <- boot_leafburn_df %>%
  mutate(fits=map(boot_samps,leafburn_boot_fit))

# Plot the results
boot_leafburn_df %>% unnest(fits) %>%
  group_by(term) %>%
  ggplot(aes(x=estimate)) + geom_histogram() + 
  facet_wrap(~term)



