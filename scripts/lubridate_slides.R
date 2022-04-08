library(tidyverse)
library(lubridate)
library(nycflights13)
library(officer)

getwd()

dt_slides <- read_pptx("slides/lubridate.pptx")

dt_slides %>% 
  layout_properties()

dt_slides %>% 
  layout_summary()


