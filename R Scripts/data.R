
library(tidyverse)
library(rvest)
library(janitor)
library(htmltab)
# Read in csv files from Kaggle - Oscar winners by demographics and information about the Oscar Winners
demographics <- read_csv("Oscars_Local/Oscars-demographics-DFE.csv") %>%
  clean_names()
oscars_overall <- read_csv("Oscars_Local/the_oscar_award.csv") %>%
  clean_names() %>%
  select(year_ceremony, category, name, film, winner) %>%
  setNames(c("year", "category", "name", "film", "winner"))

# cleaned IMDB data - code in imdb.R
imdb_titles_ratings <- readRDS("Oscars_Local/imdb_titles_ratings.rds") %>%
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
  mutate(country = str_replace(country, "\\[\\w\\]", "")) %>%
  mutate(director = str_split(director, " and ")) %>%
  mutate(row_num = 1:nrow(biff_titles)) %>%
  mutate(year = year %>% substr(1, 4) %>% as.numeric()) %>%
  mutate(win = (row_num %% 5 == 1)) %>%
  select(-row_num) %>%
  mutate(runtime = map2(film, year, ~ join_imdb_movies(.x, .y, 1)) %>% as.numeric(),
         rating = map2(film, year, ~ join_imdb_movies(.x, .y, 2)) %>% as.numeric(),
         votes = map2(film, year, ~ join_imdb_movies(.x, .y, 3)) %>% as.numeric())



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

# helper functions for fixing countries
replace_countries <- function(s) {
  replace <- c("United States"="UnitedStates", "United Kingdom"="UnitedKingdom",
               "West Germany"="WestGermany", "Czech Republic"="CzechRepublic", 
               "Hong Kong"="HongKong", "South Korea"="SouthKorea",
               "New Zealand"="NewZealand", "Czechoslovakia" = "CzechRepublic",
               "SFR Yugoslavia"="Serbia", "FR Yugoslavia"="Serbia",
               "Soviet Union"="SovietUnion")
  str_replace_all(s, replace)
}
reverse_countries <- function(v) {
  search <- c("UnitedStates"="United States", "UnitedKingdom"="United Kingdom",
              "WestGermany"="Germany", "CzechRepublic" = "Czech Republic",
              "HongKong"="Hong Kong", "SouthKorea"="South Korea",
              "NewZealand"="New Zealand", "SovietUnion"="Russia")
  str_replace_all(v, search)
}

# functions for assigning year, win from HTML table based on indices
bafta_year <- function(row) {
  ifelse(row < 73, assign_year(row-1, 4, 1983), ifelse(row < 88, assign_year(row - 73, 5, 2001), ifelse(row < 94, assign_year(row - 88, 6, 2004), assign_year(row - 94, 5, 2005))))
}

bafta_win <- function(row) {
  ifelse(row < 73, (row-1) %% 4 == 0, ifelse(row < 88, (row - 73) %% 5 == 0, ifelse(row < 94, (row - 88) %% 6 == 0, (row - 94) %% 5 == 0)))
}


palme_dor <- palme_dor %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year)) %>%
  filter(!(year %in% c(1948, 1950, 1968)))

# Here, I clean the bafta data to add year, winners, and split the country
# string into a list of countries.

bafta_year_win <- bafta %>%
  mutate(film = str_replace(film, "\\(.*\\)", "")) %>%
  mutate(producer = strsplit(producer, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) %>%
  mutate(country = country %>% replace_countries() %>% strsplit(" ")) %>%
  mutate(first_2 = substr(director, 1,2)) %>%
  filter(!(first_2 == "19" | first_2 == "20")) %>%
  mutate(row_num = 1:173) %>%
  mutate(year = bafta_year(row_num)) %>%
  mutate(win = bafta_win(row_num)) %>%
  select(-first_2, -row_num) %>%
  mutate(runtime = map2(film, year, ~ join_imdb_movies(.x, .y, 1)) %>% as.numeric(),
         rating = map2(film, year, ~ join_imdb_movies(.x, .y, 2)) %>% as.numeric(),
         votes = map2(film, year, ~ join_imdb_movies(.x, .y, 3)) %>% as.numeric())

unnest_bafta <- bafta_year_win %>%
  unnest(country) %>%
  mutate(country = country %>% reverse_countries())




# helper function for replacing weird concatenations
fix_film_name <- function(s) {
  replace <- c("of "= " of ", "Fahrenheit911" = "Farenheit 911", "IDaniel" = "I, Daniel",
               "to " = " to ", "the " = " the ", "and "= " and ", "ofthe "=" of the ", 
               "inthe " = " in the ", "tothe " = " to the ", 
               "andthe " = " and the ", "tie"="","anda "=" and a ")
  str_replace_all(s, replace)
}
fix_film_name("The Tragedyof Othello The Moorof Venice")

palme_dor_year_win <- palme_dor %>%
  mutate(film = space_string(film) %>% fix_film_name()) %>%
  mutate(country = country %>% replace_countries() %>% strsplit(" ")) %>%
  rename(director = director_s) %>%
  mutate(director = strsplit(director, " and ")) %>%
  mutate(runtime = map2(film, year, ~ join_imdb_movies(.x, .y, 1)) %>% as.numeric(),
         rating = map2(film, year, ~ join_imdb_movies(.x, .y, 2)) %>% as.numeric(),
         votes = map2(film, year, ~ join_imdb_movies(.x, .y, 3)) %>% as.numeric())

unnest_palme <- palme_dor_year_win %>%
  unnest(country) %>%
  mutate(country = country %>% reverse_countries())

# BIFF DATA CLEANING - taking away the brackets, separating the new countries
# list into a list of Countries


biff_countries_clean <- biff_countries %>%
  mutate(country = str_replace(country, "\\[\\w\\]", ""))


biff_year_clean <- biff_year %>%
  mutate(first_time = strsplit(first_time, ", "))


# function for matching/joining for similar things
join_imdb_movies <- function(movie, y, n = c(1, 2, 3)) {
  entry <- imdb_titles_ratings %>%
    filter(primary_title == movie & abs(y - start_year) < 3) %>%
    select(runtime_minutes, average_rating, num_votes) %>%
    head(1) %>%
    pull(n)
  return (entry)
}
join_imdb_movies("Missing", 2009, 1)

# create tibbles for cumulative data over time
bafta_over_time <- tibble(yr = 1983:2020) %>%
  mutate(country_total = map(yr, ~ filter(unnest_bafta, year <= .) %>%
                                 group_by(country) %>% tally() %>%
                                 arrange(desc(n))))

biff_over_time <- tibble(yr = 1960:2019) %>%
  mutate(country_total = map(yr, ~ filter(biff_titles, year <= .) %>%
                                   group_by(country) %>% tally() %>%
                                   arrange(desc(n))))
palme_over_time <- tibble(yr = 1946:2019)  %>%
  mutate(country_total = map(yr, ~ filter(unnest_palme, year <= .) %>%
                               group_by(country) %>% tally() %>%
                               arrange(desc(n))))
biff_first_time <- biff_year_clean %>%
  unnest(first_time) %>%
  mutate(decade = case_when(
    year < 1960 ~ "1950s",
    year < 1970 ~ "1960s",
    year < 1980 ~ "1970s",
    year < 1990 ~ "1980s",
    year < 2000 ~ "1990s",
    year < 2010 ~ "2000s",
    year < 2020 ~ "2010s",
    TRUE ~ "None"
    )) %>%
  select(-submissions)

# save stuff
saveRDS(bafta_year_win, file="Oscars_Local/bafta_year_win.rds")
saveRDS(palme_dor_year_win, file="Oscars_Local/palme_dor_year_win.rds")
saveRDS(biff_countries_clean, file="Oscars_Local/biff_countries_clean.rds")
saveRDS(biff_year_clean, file="Oscars_Local/biff_year_clean.rds")
saveRDS(biff_titles, file="Oscars_Local/biff_titles.rds")

saveRDS(bafta_over_time, file="Oscars_Local/bafta_over_time.rds")
saveRDS(biff_over_time, file="Oscars_Local/biff_over_time.rds")
saveRDS(palme_over_time, file="Oscars_Local/palme_over_time.rds")
saveRDS(biff_first_time, file="Oscars_Local/biff_first_time.rds")
saveRDS(imdb_titles_ratings, file="Oscars_Local/imdb_titles_ratings.rds")

