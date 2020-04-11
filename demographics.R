library(tidyverse)
library(rvest)
library(janitor)

imdb_titles <- read_tsv("Oscars_Local/name.basics.tsv")

imdb_names <- imdb_titles %>%
  select(primaryName, birthYear)
# Read in csv files from Kaggle - Oscar winners by demographics and information about the Oscar Winners
demographics <- read_csv("Oscars_Local/Oscars-demographics-DFE.csv") %>%
  clean_names() %>%
  select(person, year_of_award, movie, birthplace, date_of_birth, race_ethnicity,
         religion, sexual_orientation)
oscars_overall <- read_csv("Oscars_Local/the_oscar_award.csv") %>%
  clean_names() %>%
  select(year_ceremony, category, name, film, winner) %>%
  setNames(c("year", "category", "name", "film", "winner"))

fix_award_names <- function(s) {
  replace <- c("ACTOR IN A SUPPORTING ROLE" = "SUPPORTING ACTOR",
              "ACTOR IN A LEADING ROLE" = "ACTOR",
              "ACTRESS IN A LEADING ROLE" = "ACTRESS",
              "ACTRESS IN A SUPPORTING ROLE" = "SUPPORTING ACTRESS")
  str_replace_all(s, replace)
}
fix_winner_name <- function(s) {
  replace = c(".*by " = "",
              " and " = ", ",
              " & " = ", ")
  str_replace_all(s, replace)
}

oscars_demographics <- oscars_overall %>% 
  left_join(demographics, by=c("name"="person", "year"="year_of_award")) %>%
  mutate(religion = ifelse(religion == "Na", NA, religion)) %>%
  mutate(category = map(category, ~ fix_award_names(.))) %>%
  mutate(year_birth = map(date_of_birth, ~ ifelse(!is.na(.),
                                                  str_split(., "-"),
                                                  NA))) %>%
  mutate(year_birth = map(year_birth, ~ ifelse(!is.na(.),
                                               .[[1]][3],
                                               NA)) %>% as.numeric())


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
    TRUE ~ 1000
  )
}

oscars_demographics_full <- oscars_demographics %>% 
  left_join(imdb_names, by=c("name"="primaryName"))
oscars_demographics <- oscars_demographics_full %>%
  mutate(year_birth = ifelse(year_birth < 100, year_birth + 1900, year_birth)) %>%
  mutate(birthYear = ifelse(birthYear == "\\N",
                            100,
                            birthYear)) %>%
  mutate(birthYear = as.numeric(birthYear),
         year = as.numeric(year)) %>%
  mutate(birthYear = ifelse(!is.na(year_birth) & year_birth != birthYear,
                            year_birth,
                            birthYear)) %>%
  mutate(birthYear = map2(name, birthYear, ~ ifelse(fix_birth_years(.x) == 1000,
                                                    .y,
                                                    fix_birth_years(.x)))) %>%
  mutate(year = as.numeric(year),
         birthYear = as.numeric(birthYear)) %>%
  mutate(age = ifelse(birthYear == 100,
                      13,
                      year - birthYear)) %>%
  arrange(desc(age)) %>%
  distinct(year, name, film, .keep_all = TRUE)
oscars_demographics <- oscars_demographics %>%
  mutate(age = ifelse(age == 13 | age > 89 | age < 10,
                      NA,
                      age)) %>%
  select(-year_birth)

oscar_winners <- oscars_demographics %>%
  filter(winner)

oscar_recent <- oscars_demographics %>%
  filter(year > 1999)
