library(tidyverse)
library(janitor)
library(here)

# cases
# https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv

# test
# https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/ed270bb8-340b-41f9-a7c6-e8ef587e6d11/download/covidtesting.csv

cases <- readr::read_csv(here::here("data", "conposcovidloc.csv"))
test <- readr::read_csv(here::here("data", "covidtesting.csv"))

CANcases <- readr::read_csv(here::here("data", "cases.csv"))
CANcases <- CANcases %>%
  filter(province == "Ontario")

CANmort <- readr::read_csv(here::here("data", "mortality.csv"))
CANmort <- CANmort %>%
  filter(province == "Ontario")

# standardize names
cases <- janitor::clean_names(cases)
test <- janitor::clean_names(test)

my_count <- function(x, ...) {
  x %>%
    group_by(...) %>%
    count(sort = TRUE)
}

# Cases Data --------------------------------------------------------------
# Demographics
cases %>% 
  my_count(age_group, sort = TRUE)

cases %>% 
  my_count(client_gender, sort = TRUE)

# Epidemiologic Characteristics
cases %>% 
  my_count(case_acquisitioninfo)

cases %>% 
  my_count(outcome1)

# Geographic
cases %>% 
  my_count(reporting_phu, sort = TRUE)

cases %>% 
  my_count(reporting_phu_city, sort = TRUE)

cases %>% 
  my_count(reporting_phu_latitude, sort = TRUE)

# Time Series
cases %>% 
  my_count(accurate_episode_date, outcome1) %>% 
  ggplot(aes(x = accurate_episode_date, y = n, colour = outcome1)) +
  geom_point()


# Testing Data ------------------------------------------------------------
test %>% 
  ggplot(aes(x = reported_date, y = confirmed_negative)) +
  geom_point()

test %>% 
  ggplot(aes(x = reported_date, y = confirmed_positive)) +
  geom_point()

