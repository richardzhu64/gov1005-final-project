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
biff_over_time <- readRDS("Oscars_Local/biff_over_time.rds")
bafta_over_time <- readRDS("Oscars_Local/bafta_over_time.rds")
palme_over_time <- readRDS("Oscars_Local/palme_over_time.rds")
bafta_year_win <- readRDS("Oscars_Local/bafta_year_win.rds")


countries <- ne_countries(returnclass = "sf") %>%
  clean_names() %>%
  select(name_long, geometry) %>%
  rename(name = name_long)

# BIFF First Time Stuff
biff_first_time <- biff_first_time %>%
  mutate(first_time = str_trim(first_time))


biff_first_time_countries <- countries %>%
  left_join(biff_first_time, by=c("name" = "first_time")) %>%
  filter(!is.na(year))

first_1950s <- biff_first_time_countries %>%
  filter(decade == "1950s")
first_1960s <- biff_first_time_countries %>%
  filter(decade == "1960s")
first_1970s <- biff_first_time_countries %>%
  filter(decade == "1970s")
first_1980s <- biff_first_time_countries %>%
  filter(decade == "1980s")
first_1990s <- biff_first_time_countries %>%
  filter(decade == "1990s")
first_2000s <- biff_first_time_countries %>%
  filter(decade == "2000s")
first_2010s <- biff_first_time_countries %>%
  filter(decade == "2010s")

biff_first_time_animation <- ggplot(biff_first_time_countries) +
  geom_sf(data = countries) + 
  geom_sf(data = first_1950s, aes(fill = decade)) +
  geom_sf(data = first_1960s, aes(fill = decade)) +
  geom_sf(data = first_1970s, aes(fill = decade)) +
  geom_sf(data = first_1980s, aes(fill = decade)) +
  geom_sf(data = first_1990s, aes(fill = decade)) +
  geom_sf(data = first_2000s, aes(fill = decade)) +
  geom_sf(data = first_2010s, aes(fill = decade)) +
  labs(title = "Oscars Best International First Submission Over Time",
       caption = "Source: Wikipedia/Academy of Motion Picture Arts and Sciences",
       color = "Decade of First Submission") +
  theme_void() +
  transition_layers(layer_length = 1, transition_length = 2)

anim_save("biff_first_time_animation.gif", biff_first_time_animation)
  
biff_first_time_graph <- ggplot(biff_first_time_countries) +
  geom_sf(data = countries) +
  geom_sf(aes(fill = decade))
biff_first_time_graph

# BIFF Over Time Graph

biff_over_time_sf <- biff_over_time %>%
  mutate(country_total = map(country_total, ~ left_join(., countries,
                                            by=c("country" = "name"))
                                                                      )) %>%
  unnest(cols = c("country_total")) %>%
  mutate(geom_length = map(geometry, ~ length(.))) %>%
  filter(geom_length != 0) %>%
  select(-geom_length) %>%
  st_as_sf(sf_column_name = "geometry")

biff_over_time_animation <- ggplot(biff_over_time_sf) +
  geom_sf(data = countries) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "plasma",
                       direction = -1) +
  labs(title = "Oscars Best International Film Nominations by Country Over Time",
       caption = "Source: Wikipedia/Academy of Motion Picture Arts and Sciences",
       fill = "Nominations",
       subtitle = "Year: {frame_time}") +
  theme_void() +
  transition_time(yr)

slower <- animate(biff_over_time_animation, duration = 10,
        fps = 6)
anim_save("biff_over_time_animation.gif", slower)

biff_winners <- biff_titles %>%
  filter(win) 
biff_winners_sf <- biff_winners %>%
  mutate(country_total = map(year, ~ filter(biff_winners, year <= .) %>%
                               group_by(country) %>% tally() %>%
                               arrange(desc(n))))
biff_winners_sf <- biff_winners_sf %>%
  mutate(country_total = map(country_total, ~ left_join(., countries,
                                                        by=c("country" = "name"))
  )) %>%
  unnest() %>%
  mutate(geom_length = map(geometry, ~ length(.))) %>%
  filter(geom_length != 0) %>%
  select(-geom_length) %>%
  st_as_sf(sf_column_name = "geometry")

biff_winners_over_time_animation <- ggplot(biff_winners_sf) +
  geom_sf(data = countries) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "plasma",
                       direction = -1) +
  labs(title = "Oscars Best International Film Winners by Country Over Time",
       caption = "Source: Wikipedia/Academy of Motion Picture Arts and Sciences",
       fill = "Winners",
       subtitle = "Year: {frame_time}") +
  theme_void() +
  transition_time(year)
  
slower_winners <- animate(biff_winners_over_time_animation, duration = 10, fps = 6)
anim_save("biff_winners_over_time_animation.gif", slower_winners)

# PALME DOR ANIMATION

palme_dor_long <- palme_over_time %>%
  mutate(country_total = map(country_total, ~ left_join(., countries,
                                                        by=c("country" = "name"))
  )) %>%
  unnest() %>%
  mutate(geom_length = map(geometry, ~ length(.))) %>%
  filter(geom_length != 0) %>%
  select(-geom_length) %>%
  st_as_sf(sf_column_name = "geometry")
palme_dor_long <- palme_dor_long %>%
  filter(yr >= 1960)

palme_over_time_animation <- ggplot(palme_dor_long) +
  geom_sf(data = countries) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "plasma",
                       direction = -1) +
  labs(title = "Cannes Film Festival (Palme D'Or) by Country Over Time",
       caption = "Source: Cannes Film Festival",
       fill = "Winners",
       subtitle = "Year: {frame_time}") +
  theme_void() +
  transition_time(yr)
slower_palme <- animate(palme_over_time_animation, nframes = 60, duration = 10)
anim_save("palme_over_time_animation.gif", slower_palme)

# BAFTA NOMINATIONS ANIMATION

bafta_sf <- bafta_over_time %>%
  mutate(country_total = map(country_total, ~ left_join(., countries,
                                                        by=c("country" = "name"))
  )) %>%
  unnest() %>%
  mutate(geom_length = map(geometry, ~ length(.))) %>%
  filter(geom_length != 0) %>%
  select(-geom_length) %>%
  st_as_sf(sf_column_name = "geometry")

bafta_over_time_animation <- ggplot(bafta_sf) +
  geom_sf(data = countries) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "plasma",
                       direction = -1) +
  labs(title = "British Academy (BAFTA) Best Foreign Language Film by Country Over Time",
       caption = "Source: British Academy Film Awards",
       fill = "Nominations",
       subtitle = "Year: {frame_time}") +
  theme_void() +
  transition_time(yr)
slower_bafta <- animate(bafta_over_time_animation, nframes = 38, duration = 10)
anim_save("bafta_over_time_animation.gif", slower_bafta)


reverse_countries <- function(v) {
  search <- c("UnitedStates"="United States", "UnitedKingdom"="United Kingdom",
              "WestGermany"="Germany", "CzechRepublic" = "Czech Republic",
              "HongKong"="Hong Kong", "SouthKorea"="South Korea",
              "NewZealand"="New Zealand", "SovietUnion"="Russia")
  str_replace_all(v, search)
}

# BAFTA WINNERS ANIMATION

unnest_bafta <- bafta_year_win %>%
  unnest(country) %>%
  mutate(country = country %>% reverse_countries())


bafta_winners <- unnest_bafta %>%
  filter(win)
bafta_winners_sf <- bafta_winners %>%
  mutate(country_total = map(year, ~ filter(bafta_winners, year <= .) %>%
                               group_by(country) %>% tally() %>%
                               arrange(desc(n))))
bafta_winners_sf <- bafta_winners_sf %>%
  mutate(country_total = map(country_total, ~ left_join(., countries,
                                                        by=c("country" = "name"))
  )) %>%
  rename(country_name = country) %>%
  unnest(country_total) %>%
  mutate(geom_length = map(geometry, ~ length(.))) %>%
  filter(geom_length != 0) %>%
  select(-geom_length) %>%
  st_as_sf(sf_column_name = "geometry")

bafta_winners_sf <- bafta_winners_sf %>%
  distinct(year, country, n, .keep_all = TRUE) %>%
  mutate(year = as.numeric(year))

bafta_winners_over_time_animation <- ggplot(bafta_winners_sf) +
  geom_sf(data = countries) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "plasma",
                       direction = -1) +
  labs(title = "British Academy (BAFTA) Best Foreign Language Film by Country Over Time",
       caption = "Source: British Academy Film Awards",
       fill = "Nominations",
       subtitle = "Year: {frame_time}") +
  theme_void() +
  transition_time(year)
slower_bafta_winners <- animate(bafta_winners_over_time_animation, nframes = 38,
                                duration = 10)
anim_save("bafta_winners_over_time_animation.gif", slower_bafta_winners)
