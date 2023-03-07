library(tidyverse)
library(fpp3)
library(openxlsx)

gafa_stock %>% 
  filter(Symbol %in% c("AAPL", "FB") & year(Date) == 2017)

## Working with tourism data (INTRO TO FINANCIAL TIME SERIES - 1st class)
## Exercise 1 and 2

tourism <- read.xlsx("https://robjhyndman.com/data/tourism.xlsx")

tourism <- tourism %>% 
  mutate(Quarter = yearquarter(Quarter)) %>% 
  as_tsibble(index= Quarter, key = c(Region, State, Purpose))

## Exercise 3
tourism %>% 
  features(Trips, list(mean=mean)) %>% 
  arrange(desc(mean))

tourism %>% 
  as_tsibble() %>% 
  group_by(Region, Purpose) %>% 
  summarise(mean = mean(Trips)) %>% 
  arrange(desc(mean))

tourism %>% 
  filter(Region == "Adelaide", State == "South Australia", Purpose == "Business") %>% 
  as_tibble()

## Exercise 4

tourism %>% 
  group_by(State) %>% 
  summarise(Trips = sum(Trips))
