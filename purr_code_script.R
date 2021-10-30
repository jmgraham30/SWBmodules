library(tidyverse)
library(broom)
library(faraway)

set.seed(42)

x <- rnorm(35,10,6.7)

my_boot_samp <- function(i){
  return(sample(x,replace=TRUE))
}

boot_samps <- map(1:500,my_boot_samp) 

boot_means <- map_dbl(boot_samps,mean)

quantile(boot_means,probs = c(0.025,0.975))

mean(x) + 1.96*c(-1,1)*(sd(x)/sqrt(length(x)))

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



