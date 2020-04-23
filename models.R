library(tidyverse)
library(rvest)
library(janitor)
library(gender)
library(broom)
library(countrycode)

# OSCARS DEMOGRAPHICS MODELS
gendered_awards <- c("ACTOR", "SUPPORTING ACTOR", "ACTRESS", "SUPPORTING ACTRESS")

oscar_gender_age <- readRDS("Oscars_Local/oscar_gender_age.rds")
oscar_no_gendered <- oscar_gender_age %>%
  filter(!(category %in% gendered_awards))

oscar_recent <- oscar_no_gendered %>%
  filter(year >= 2000)



oscar_winners_dems <- oscar_gender_age %>%
  filter(winner)

oscar_gender_data <- oscar_gender_age %>%
  filter(!is.na(gender)) %>%
  group_by(year, gender) %>%
  summarize(n = n()) %>%
  mutate(frequency = 100 * n / sum(n))

oscar_gender_graph <- ggplot(oscar_gender_data, aes(x = year, y = frequency, color = gender)) +
  geom_line() +
  scale_color_manual(values = c("pink", "blue")) +
  theme_classic() +
  labs(title = "Oscar Nominees by Gender, All Categories",
       x = "Year",
       y = "Proportion (in Percent)",
       color = "Gender",
       caption = "Source: AMPAS")
oscar_gender_graph

oscar_no_gender_data <- oscar_no_gendered %>%
  filter(!is.na(gender)) %>%
  group_by(year, gender) %>%
  summarize(n = n()) %>%
  mutate(frequency = 100 * n / sum(n))

oscar_no_gender_graph <- ggplot(oscar_no_gender_data, aes(x = year, y = frequency, color = gender)) +
  geom_line() +
  scale_color_manual(values = c("pink", "blue")) +
  theme_classic() +
  labs(title = "Oscar Nominees by Gender, Non-Gendered Categories",
       subtitle = "Excluding Actor & Actress Nominations",
       x = "Year",
       y = "Proportion (in Percent)",
       color = "Gender",
       source = "AMPAS")
oscar_no_gender_graph


no_gendered_model <- oscar_no_gendered %>%
  lm(winner ~ age + gender * year, data = .) %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high)
gender_model <- oscar_gender_age %>%
  lm(winner ~ age + gender * year, data = .) %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high)

recent_model <- oscar_recent %>%
  lm(winner ~ age + gender * year, data = .) %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high)

# OSCARS BIFF COMPARED TO OTHERS GRAPHS

reverse_countries <- function(v) {
  search <- c("UnitedStates"="United States", "UnitedKingdom"="United Kingdom",
              "WestGermany"="Germany", "CzechRepublic" = "Czech Republic",
              "HongKong"="Hong Kong", "SouthKorea"="South Korea",
              "NewZealand"="New Zealand", "SovietUnion"="Russia")
  str_replace_all(v, search)
}
palme_dor <- readRDS("Oscars_Local/palme_dor_year_win.rds")
oscars <- readRDS("Oscars_Local/biff_titles.rds")
baftas <- readRDS("Oscars_Local/bafta_year_win.rds")

unnest_palme_country <- palme_dor %>%
  unnest(country) %>%
  mutate(country = country %>% reverse_countries()) %>%
  mutate(continent = map(country, ~ countrycode(sourcevar = ., 
                                                origin = "country.name",
                                                destination = "continent"))) %>%
  mutate(continent = as.character(continent))


unnest_baftas_country <- baftas %>%
  unnest(country) %>%
  mutate(country = country %>% reverse_countries()) %>%
  mutate(continent = map(country, ~ countrycode(sourcevar = ., 
                                              origin = "country.name",
                                              destination = "continent"))) %>%
  mutate(continent = as.character(continent))

unnest_oscars_country <- oscars %>%
  mutate(continent = map(country, ~ countrycode(sourcevar = ., 
                                                origin = "country.name",
                                                destination = "continent"))) %>%
  mutate(continent = as.character(continent)) %>%
  mutate(continent = ifelse(is.na(continent), "Europe", continent))

oscars_continent_data <- tibble(yr = 1960:2019) %>%
  mutate(country_total = map(yr, ~ filter(unnest_oscars_country, year <= .) %>%
                               group_by(continent) %>% tally())) %>%
  unnest(country_total) %>%
  group_by(yr) %>%
  mutate(frequency = 100 * n / sum(n)) %>%
  filter(!is.na(continent))

bafta_continent_data <- tibble(yr = 1983:2020) %>%
  mutate(country_total = map(yr, ~ filter(unnest_baftas_country, year <= .) %>%
                               group_by(continent) %>% tally())) %>%
  unnest(country_total) %>%
  group_by(yr) %>%
  mutate(frequency = 100 * n / sum(n)) %>%
  filter(!is.na(continent))

palme_continent_data <- tibble(yr = 1946:2019) %>%
  mutate(country_total = map(yr, ~ filter(unnest_palme_country, year <= .) %>%
                               group_by(continent) %>% tally())) %>%
  unnest(country_total) %>%
  group_by(yr) %>%
  mutate(frequency = 100 * n / sum(n)) %>%
  filter(!is.na(continent))


  
bafta_continent_graph <- bafta_continent_data %>%
  ggplot(aes(x = yr, y = frequency, color = continent)) +
  geom_line() +
  theme_classic() +
  labs(title = "BAFTA Best Foreign Film Nominees By Continent Over Time",
       subtitle = "Percentage of Films Nominated From 1983 to Year",
       x = "Year",
       y = "Proportion (in Percent)",
       color = "Continent",
       source = "BAFTA")
bafta_continent_graph

palme_continent_graph <- palme_continent_data %>%
  ggplot(aes(x = yr, y = frequency, color = continent)) +
  geom_line() +
  theme_classic() +
  labs(title ="Palme D'Or Winners By Continent Over Time",
       subtitle = "Percentage of Palme D'Or Winners From 1946 to Year",
       x = "Year",
       y = "Proportion (in Percent)",
       color = "Continent",
       source = "Cannes Film Festival")
palme_continent_graph

oscars_continent_graph <- oscars_continent_data %>%
  ggplot(aes(x = yr, y = frequency, color = continent)) +
  geom_line() +
  theme_classic() +
  labs(title ="Oscar Best International Film By Continent Over Time",
       subtitle = "Percentage of BIFF Nominees From 1960 to Year",
       x = "Year",
       y = "Proportion (in Percent)",
       color = "Continent",
       source = "AMPAS")
oscars_continent_graph

# OSCARS COMPARED TO OTHERS MODELS

bafta_continent_model <- unnest_baftas_country %>%
  glm(win ~ continent, data = ., family = "binomial") %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(estimate = plogis(estimate) %>% round(3),
         conf.low = plogis(conf.low) %>% round(3),
         conf.high = plogis(conf.high) %>% round(3))

oscars_continent_model <- unnest_oscars_country %>%
  glm(win ~ continent, data = ., family = "binomial") %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(estimate = plogis(estimate) %>% round(3),
         conf.low = plogis(conf.low) %>% round(3),
         conf.high = plogis(conf.high) %>% round(3))

bafta_popularity_model <- baftas %>%
  filter(!is.na(rating)) %>%
  glm(win ~ rating + votes + runtime, data = ., family = "binomial") %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(estimate = plogis(estimate) %>% round(3),
         conf.low = plogis(conf.low) %>% round(3),
         conf.high = plogis(conf.high) %>% round(3))

oscars_popularity_model <- oscars %>%
  filter(!is.na(rating)) %>%
  glm(win ~ rating + votes + runtime, data = ., family = "binomial") %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(estimate = plogis(estimate) %>% round(3),
         conf.low = plogis(conf.low) %>% round(3),
         conf.high = plogis(conf.high) %>% round(3))
  