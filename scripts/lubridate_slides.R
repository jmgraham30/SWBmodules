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

dt_slides <- dt_slides %>%
  add_slide()

dt_slides <- dt_slides %>%
  ph_with(value="A New Slide",location=ph_location_type(type="title"))

print(dt_slides,target="updated_slides.pptx")
