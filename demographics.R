library(tidyverse)
library(rvest)
library(janitor)
library(gender)
library(broom)

fix_birth_years <- function(s) {
  case_when (
    s == "John Williams" ~ 1932,
    s == "Amy Adams" ~ 1974,
    s == "Anthony Powell" ~ 1935,
    s == "Arthur Miller" ~ 1915,
    s == "Barry Levinson" ~ 1942,
    s == "Bernard Herrmann" ~ 1911,
    s == "Bill Thomas" ~ 1921,
    s == "Billy Williams" ~ 1929,
    s == "Charles Lang" ~ 1902,
    s == "David Lynch" ~ 1946,
    s == "Elizabeth Taylor" ~ 1932,
    s == "George Barnes" ~ 1892,
    s == "George Martin" ~ 1926,
    s == "George Stevens" ~ 1904,
    s == "Ingrid Bergman" ~ 1915,
    s == "Jim Clark" ~ 1931,
    s == "John Barry" ~ 1933,
    s == "John Bloom" ~ 1935,
    s == "John Seale" ~ 1942,
    s == "John Singleton" ~ 1968,
    s == "Lee Grant" ~ 1927,
    s == "Mark Baker" ~ 1959,
    s == "Michael Chapman" ~ 1935,
    s == "Michael O'Connor" ~ 1965,
    s == "Michael Shannon" ~ 1974,
    s == "Paul Smith" ~ 1906,
    s == "Richard Brooks" ~ 1912,
    s == "Robert Anderson" ~ 1960,
    s == "Robin Williams" ~ 1951,
    s == "Sidney Franklin" ~ 1893,
    s == "Victor Young" ~ 1900,
    s == "Will Smith" ~ 1968,
    s == "William Reynolds" ~ 1910,
    s == "Woody Allen" ~ 1935,
    s == "John Ford" ~ 1894,
    s == "Robert Richardson" ~ 1955,
    s == "George Miller" ~ 1945,
    TRUE ~ 1000
  )
}
oscars_demographics_clean <- readRDS("oscars_demographics_clean.rds") %>%
  mutate(birthYear = map2(name, birthYear, ~ ifelse(fix_birth_years(.x) == 1000,
                                                    .y,
                                                    fix_birth_years(.x)))) %>%
  mutate(year = as.numeric(year),
         birthYear = as.numeric(birthYear))

birth_years <- oscars_demographics_clean %>%
  filter(!is.na(birthYear) & birthYear > 1000)
birth_years <- birth_years %>%
  select(name, birthYear) %>%
  mutate(name = as.character(name)) %>%
  distinct(name, birthYear, .keep_all = TRUE)


oscars_data_analysis <- oscars_demographics_clean %>%
  mutate(names = map(name, ~ str_split(., ", ")[[1]])) %>%
  unnest() %>%
  mutate(first_name = map(names, ~ word(., 1))) %>%
  select(-birthYear, -age)

oscar_birth_join <- oscars_data_analysis %>%
  left_join(birth_years, by=c("names" = "name")) %>%
  mutate(age = year - birthYear) %>%
  filter(age > 9 & age < 90)

oscar_names <- as.vector(unlist(oscars_data_analysis$first_name)) %>%
  unique()
oscar_genders <- gender(oscar_names, method = "ssa")
oscar_genders <- oscar_genders %>%
  mutate(name = name %>% as.character())

gendered_awards <- c("ACTOR", "SUPPORTING ACTOR", "ACTRESS", "SUPPORTING ACTRESS")


oscar_gender_age <- oscar_birth_join %>%
  select(year, category, name, film, winner, birthYear, age, names, first_name) %>%
  mutate(first_name = as.character(first_name)) %>%
  left_join(oscar_genders, by =c("first_name"="name"))

saveRDS(oscar_gender_age, "Oscars_Local/oscar_gender_age.rds")

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
