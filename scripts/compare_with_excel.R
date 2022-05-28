# load packages
library(tidyverse)
library(lubridate)

# import data
data_folder <- "./Data/"
data_file <- paste0(data_folder,"Monthly Employment.csv")
food_services <- read_csv(data_file)

r_package_downloads <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-30/r-downloads.csv")

# examine data variable types
food_services %>% 
  glimpse()


r_package_downloads %>%
  glimpse()

# comparison with excel
food_services %>% 
  mutate(MonthYearDate=lubridate::mdy(`Month of Year`)) %>% 
  glimpse()
