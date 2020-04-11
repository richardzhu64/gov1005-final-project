# load packages

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(janitor)
library(gganimate)
library(transformr)

# read in datasets
biff_first_time <- readRDS("Oscars_Local/biff_first_time.rds")
biff_first_time <- biff_first_time %>%
  mutate(first_time = str_trim(first_time))

countries <- ne_countries(returnclass = "sf") %>%
  clean_names()

biff_first_time_countries <- countries %>%
  left_join(biff_first_time, by=c("name" = "first_time")) %>%
  filter(!is.na(year))
ggplot(biff_first_time_countries) + 
  geom_sf(aes(fill = decade)) +
  labs(title = "Oscars Best International First Submission Over Time",
       caption = "Source: Wikipedia/Academy of Motion Picture Arts and Sciences",
       color = "Decade of First Submission") +
  theme_void() +
  transition_reveal(along=year)
