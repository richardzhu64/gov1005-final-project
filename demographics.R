library(tidyverse)
library(rvest)
library(janitor)
library(gender)
oscars_demographics_clean <- readRDS("oscars_demographics_clean.rds")
oscars_data_analysis <- oscars_demographics_clean %>%
  mutate(names = map(name, ~ str_split(., ", ")[[1]])) %>%
  unnest() %>%
  mutate(first_name = map(names, ~ word(., 1)))
oscar_gender <- oscars_data_analysis %>%
  mutate(gender_estimate = map2(first_name, year, ~ gender(.x,
                                                           year = c(year - 10, year + 10),
                                                           method = "ssa")))
