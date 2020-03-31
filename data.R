
library(tidyverse)
library(rvest)
library(janitor)
library(htmltab)
# Read in csv files from Kaggle - Oscar winners by demographics and information about the Oscar Winners
demographics <- read_csv("raw-data/Oscars-demographics-DFE.csv") %>%
  clean_names()
oscars_overall <- read_csv("raw-data/the_oscar_award.csv") %>%
  clean_names() %>%
  select(year_ceremony, category, name, film, winner) %>%
  setNames(c("year", "category", "name", "film", "winner"))

# cleaned IMDB data - code in imdb.R
imdb_titles_ratings <- read_csv("raw-data/imdb_titles_ratings.csv") %>%
  clean_names()

# Cannes and BAFTA information
cannes_html <- read_html("https://en.wikipedia.org/wiki/Palme_d%27Or") %>%
  html_nodes("table")
bafta_tables <- read_html("https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Film_Not_in_the_English_Language") %>%
  html_nodes("table")

# Oscars info
oscars_biff_overall <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_number_of_Academy_Awards_for_Best_International_Feature_Film") %>%
  html_nodes("table")



# Use rvest package to scrape data from Wikipedia from Cannes Film Festival
# Cannes Data - Palme D'Or

palme_dor <- cannes_html[[2]] %>%
  html_table(fill=TRUE) %>%
  clean_names() %>%
  mutate(film = str_replace_all(film, "[^[:alnum:]]", ""))


# BAFTA Data - Best International Feature Film

bafta_1980s <- bafta_tables[[2]] %>% html_table()
bafta_1990s <- bafta_tables[[3]] %>% html_table()
bafta_2000s <- bafta_tables[[4]] %>% html_table()
bafta_2010s <- bafta_tables[[5]] %>% html_table(fill=TRUE)
bafta_2020s <- bafta_tables[[6]] %>% html_table(fill = TRUE)
colnames(bafta_2010s) <- c("Film", "Director(s)", "Producer(s)", "Country", "trash1", "trash2")
bafta_2010s <- bafta_2010s %>%
  select(1:4)
colnames(bafta_2020s) <- c("Film", "Director(s)", "Producer(s)", "Country", "trash1")
bafta_2020s <- bafta_2020s %>%
  select(1:4)


# compiled bafta data
bafta <- do.call("rbind", list(bafta_1980s, bafta_1990s, 
                               bafta_2000s, bafta_2010s, bafta_2020s))
colnames(bafta) <- c("film", "director", "producer", "country")


# Oscars BIFF
# Overall statistics for BIFF countries, submissions, nominations

# Function to take out &&& and + from the biff_countries entries
replace_and <- function(s) {
  as.numeric(str_replace_all(s, "[^[:alnum:]]", ""))
}

biff_countries <- oscars_biff_overall[[1]] %>% 
  html_table() %>%
  setNames(c("country", "winners", "nominations", "submissions")) %>%
  mutate(winners = replace_and(winners),
         nominations = replace_and(nominations),
         submissions = replace_and(submissions))

biff_year <- oscars_biff_overall[[2]] %>% html_table() %>%
  setNames(c("year", "ceremony", "submissions", "first_time")) %>%
  select(year, submissions, first_time)

# Read in actual BIFF nominees and winners

biff_url <- "https://en.wikipedia.org/wiki/List_of_Academy_Award_winners_and_nominees_for_Best_International_Feature_Film"
biff_tables <- biff_url %>% read_html() %>% html_nodes("table")


biff_1960s <- biff_tables[[4]] %>% html_table(fill = TRUE)
biff_1970s <- biff_tables[[5]] %>% html_table(fill = TRUE)
biff_1980s <- biff_tables[[6]] %>% html_table(fill = TRUE)
biff_1990s <- biff_tables[[7]] %>% html_table(fill = TRUE)
biff_2000s <- biff_tables[[8]] %>% html_table(fill = TRUE)
biff_2010s <- biff_url %>%
  htmltab(9, rm_nodata_cols = F) %>%
  replace_na(list(Notes = "", "Term-limited?" = "")) %>%
  `rownames<-` (seq_len(nrow(.)))
  
biff_titles <- do.call("rbind", list(biff_1960s, biff_1970s, biff_1980s,
                                     biff_1990s, biff_2000s, biff_2010s))
colnames(biff_titles) <- c("year", "film", "original_title", "director", 
                           "country","language")
# add winner column
biff_titles <- biff_titles %>%
  mutate(row_num = 1:nrow(biff_titles)) %>%
  mutate(win = (row_num %% 5 == 1)) %>%
  select(-row_num)
# IMDB Title Basics - how to get years, ratings, number of votes

clean_imdb_titles <- imdb_titles %>%
  filter(titleType == "movie") %>%
  select(tconst, primaryTitle, originalTitle, startYear, runtimeMinutes) %>%
  rename(year = startYear) %>%
  clean_names()

clean_imdb_ratings <- imdb_ratings %>%
  clean_names()

clean_imdb <- clean_imdb_titles %>%
  left_join(clean_imdb_ratings, by="tconst")


# list of countries with spaces
long_countries <- c("South Korea", "West Germany", "Saudi Arabia", "Hong Kong")

split_countries <- function(countries) {
  case_when(
    grepl(countries, long_countries) ~ countries,
    len(countries) == 1 ~ strsplit(countries, " "),
    TRUE ~ countries
  )
}

# helper function for years
assign_year <- function(row, mod, init) {
  init + floor(row/mod)
}

# functions for assigning year, win from HTML table based on indices
bafta_year <- function(row) {
  ifelse(row < 72, assign_year(row, 4, 1983), ifelse(row < 87, assign_year(row - 72, 5, 2001), ifelse(row < 93, assign_year(row - 87, 6, 2004), assign_year(row - 93, 5, 2005))))
}

bafta_win <- function(row) {
  ifelse(row < 72, row %% 4 == 0, ifelse(row < 87, (row - 72) %% 5 == 0, ifelse(row < 93, (row - 87) %% 6 == 0, (row - 93) %% 5 == 0)))
}


palme_dor <- palme_dor %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year))

# Here, I clean the bafta data to add year, winners, and split the country
# string into a list of countries.

bafta_year_win <- bafta %>%
  mutate(first_2 = substr(director, 1,2)) %>%
  filter(!(first_2 == "19" | first_2 == "20")) %>%
  mutate(row_num = 1:173) %>%
  mutate(year = bafta_year(row_num)) %>%
  mutate(win = bafta_win(row_num)) %>%
  select(-first_2, -row_num) %>%
  mutate(country = strsplit(country, "\n"))


# helper function to split strings with spaces 
space_string <- function(s) {
  gsub("([a-z])([A-Z])", "\\1 \\2", s)
  
}
palme_dor_year_win <- palme_dor %>%
  mutate(country = strsplit(country, " ")) %>%
  mutate(film = space_string(film))


# BIFF DATA CLEANING - taking away the brackets, separating the new countries
# list into a list of Countries

remove_brackets <- function(s) {
  l = length(s)
  ifelse(grepl("\\[", s), substr(s, 1, l-2), s)
  }

biff_countries_clean <- biff_countries %>%
  mutate(country = str_replace(country, "\\[\\w\\]", ""))

biff_year_clean <- biff_year %>%
  mutate(first_time = strsplit(first_time, ", "))

