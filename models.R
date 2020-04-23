library(tidyverse)
library(rvest)
library(janitor)
library(gender)
library(broom)

# OSCARS DEMOGRAPHICS MODELS
gendered_awards <- c("ACTOR", "SUPPORTING ACTOR", "ACTRESS", "SUPPORTING ACTRESS")

oscar_gender_age <- readRDS("Oscars_Local/oscar_gender_age.rds")
oscar_no_gendered <- oscar_gender_age %>%
  filter(!(category %in% gendered_awards))



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
  lm(winner ~ age * gender, data = .) %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high)
gender_model <-  oscar_no_gendered %>%
  lm(winner ~ age * gender, data = .) %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high)

# OSCARS BIFF COMPARED TO OTHERS MODEL

reverse_countries <- function(v) {
  search <- c("UnitedStates"="United States", "UnitedKingdom"="United Kingdom",
              "WestGermany"="Germany", "CzechRepublic" = "Czech Republic",
              "HongKong"="Hong Kong", "SouthKorea"="South Korea",
              "NewZealand"="New Zealand", "SovietUnion"="Russia")
  str_replace_all(v, search)
}
palme_dor <- readRDS("palme_dor_year_win.rds")
oscars <- readRDS("biff_titles.rds")
baftas <- readRDS("bafta_year_win")