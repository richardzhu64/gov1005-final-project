source("data.R")

library(tidyverse)
library(rvest)
library(janitor)

# IMDb stuff
imdb_titles <- read_tsv("raw-data/title.basics.tsv")
imdb_ratings <- read_tsv("raw-data/title.ratings.tsv")
imdb_crew <- read_tsv("raw-data/title.crew.tsv")

imdb_titles_ratings <- imdb_titles %>%
  left_join(imdb_ratings, by="tconst")

# IMDB Data Cleaning
biff_names <- biff_titles %>% pull(film)
bafta_names <- bafta_year_win %>% pull(film)
palme_dor_names <- palme_dor_year_win %>% pull(film)
movie_names <- unique(c(biff_names, bafta_names, palme_dor_names))

imdb_titles_filtered <- imdb_titles %>% 
  filter(primaryTitle %in% movie_names) %>%
  filter(titleType == "movie") %>%
  select(tconst, primaryTitle, originalTitle, startYear, runtimeMinutes) %>%
  clean_names()

title_codes <- imdb_titles_filtered %>% pull(tconst)