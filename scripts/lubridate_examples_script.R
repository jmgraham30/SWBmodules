library(tidyverse)
library(lubridate)
library(nycflights13)

numeric_values <- c(1.2,6.5,8.1)
class(numeric_values)

character_values <- c("dog","cat","bird")
class(character_values)

# https://datausa.io/profile/naics/restaurants-food-services
data_folder <- "../Data/"
data_file <- paste0(data_folder,"Monthly Employment.csv")
food_services <- read_csv(data_file)
r_package_downloads <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-30/r-downloads.csv")

head(food_services)

glimpse(food_services)

glimpse(r_package_downloads)

r_package_downloads <- r_package_downloads %>% mutate(date_time=ymd(date) + hms(time))
glimpse(r_package_downloads)

(ymd("2022-03-06"))
(mdy("March 6th, 2022"))
(dmy("06-March-2022"))
(ymd(20220306))


(ymd_hms("2022-03-06 08:58:31"))
(mdy_hm("03/06/2022 08:58"))

glimpse(food_services)

food_services %>% 
  mutate(year=year(Date),month=month(Date),day=day(Date)) %>%
  glimpse()

food_services %>% mutate(year=year(Date),month=month(Date),day=day(Date)) %>%
  ggplot(aes(x=year,y=`NSA Employees Growth`)) + geom_point()


food_services %>% 
  mutate(year=year(Date),month=month(Date),day=day(Date)) %>% 
  group_by(year) %>% 
  summarise(mean_growth=mean(`NSA Employees Growth`))


flights %>% 
  select(year, month, day, hour, minute)


make_date(year=2022,month=03,day=10)


flights %>% 
  select(year, month, day, hour, minute) %>%
  mutate(flight_time=make_datetime(year,month,day,hour,minute))



flights %>% 
  select(year, month, day, hour, minute) %>%
  mutate(flight_time=make_datetime(year,month,day,hour,minute),weekday=wday(flight_time)) %>%
  group_by(weekday) %>% 
  summarise(n=n())


flights %>% 
  select(year, month, day, hour, minute) %>%
  mutate(flight_time=make_datetime(year,month,day,hour,minute),weekday=wday(flight_time,label=TRUE,abbr=FALSE)) %>%
  group_by(weekday) %>% 
  summarise(n=n())


flights %>% 
  select(year, month, day, hour, minute) %>%
  mutate(flight_time=make_datetime(year,month,day,hour,minute),weekday=wday(flight_time,label=TRUE,abbr=FALSE)) %>%
  ggplot(aes(x=weekday)) + 
  geom_bar()

flights %>% 
  select(year, month, day, hour, minute) %>%
  mutate(flight_time=make_datetime(year,month,day,hour,minute),weekday=wday(flight_time,label=TRUE,abbr=FALSE)) %>%
  group_by(weekday) %>% 
  summarise(n=n()) %>% select(n) %>% chisq.test(p=c(1/7,1/7,1/7,1/7,1/7,1/7,1/7))


