
library(shinythemes)
library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(janitor)
library(broom)
library(scales)
library(patchwork)
library(animation)

biff_titles <- readRDS("biff_titles.rds")
bafta_year_win <-  readRDS("bafta_year_win.rds")
palme_dor_year_win <- readRDS("palme_dor_year_win.rds")
oscar_demographics <- readRDS("csv_demographics.rds")
countries <- ne_countries(returnclass = "sf") %>%
    clean_names() %>%
    select(name_long, geometry) %>%
    rename(name = name_long)

reverse_countries <- function(v) {
    search <- c("UnitedStates"="United States", "UnitedKingdom"="United Kingdom",
                "WestGermany"="Germany", "CzechRepublic" = "Czech Republic",
                "HongKong"="Hong Kong", "SouthKorea"="South Korea",
                "NewZealand"="New Zealand", "SovietUnion"="Russia")
    str_replace_all(v, search)
}

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("lumen"), "Oscars So Local?: Film Awards by Country and Demographics",
    tabPanel("Home",
             h2("Welcome!", align = "center"),
             HTML("<img src = 'https://www.rd.com/wp-content/uploads/2020/01/GettyImages-927288148.jpg' align = 'center' height = '70%' width = '100%'>"),
             h3("Oscars So Local: Oscars Diversity Over Time"),
             
             p("In 2019, Bong Joon Ho, the South Korean director of recent Best 
               Picture winner Parasite, said in an interview that “the Oscars 
               are not an international film festival. They’re very local.” 
               Of course, soon after his interivew, Parasite took the Oscars 
               by storm with 6 nominations and 4 eventual wins, dominating the 
               February award show with wins in major categories of Best Picture,
               Best Director, Best International Feature, and Best Original 
               Screenplay. Parasite was the first non-English movie to win the 
               Best Picture honor, symbolic of the Oscars' recent shift towards
               more internationalism and diversity. Indeed, historically the 
               Oscars have had a reputation of being monolithic and focusing on
               the stories of a select few Americans. Bong's reference to the 
               Oscars as local was preceded by the #OscarsSoWhite movement
               that spotlighted the lack of minority representation in Oscar 
               nominations and membership. Correspondingly, I believe that it 
               would be interesting to analyze the demographics and geography of
               the Oscars' awards over time, to see if there was a broader 
               context of diversification that accompanied Parasite's historic win."),
             br(),
             p("When Bong spoke about the Oscars being local, he later 
               contextualized that he was comparing the Oscars to other famous 
               international film festivals such as the Cannes Film Festival and
               Venice Film Festival. As a result, I wanted to compare the Oscars'
               awards of Best International Feature Film with the equivalent of 
               Best Picture at Cannes - the Palme d'Or. Additionally, I compared
               the BIFF award at the Oscars with its equivalent at the BAFTAs - 
               the British film academy. Moreover, I wanted to see how different
               nations progressed in their film industries over time through 
               investigating the amount of submissions and nominations for 
               International Feature Film. As a result, I also looked into the 
               winners, nominations, and submissions of each country for the 
               Oscars' BIFF category from web scraping on Wikipedia. I wanted to
               make a visual representation of film's development all around the
               world, and looking at these datasets allowed me to do so. 
               Finally, I wanted to look into the demographics of the Oscars' 
               nominations and winners over time. Regressing the Oscars 
                eventual results over various factors provides interesting insights
               into winners and nominees over time. Thus, I utilized online
               data from Kaggle to download demographic information of Oscars 
               nominees historically to see the progress of representation over 
               time.")
             ),
    tabPanel("Oscar Best International Film",
             sidebarLayout(
                 sidebarPanel(
                     h4("About"),
                     p("This graph displays information regarding countries'
                        Oscar Best International Feature Film submissions,
                        nominations, and winners."),
                     selectInput(inputId = "biff_input",
                                 label = "Select Variable",
                                 choices = c("All", "Submissions", "Nominations", "Winners")
                     )),
                     
             mainPanel(
                 plotOutput("oscar_countries")
             )
             )),
    tabPanel("Animations Over Time",
             h2("Animations Over Time"),
             h4("This page displays animations of various awards over time."),
             HTML(readLines("graph_animations.html"))
    ),
    tabPanel("Oscars vs. Film Awards",
             sidebarLayout(
                sidebarPanel(
                    h4("About"),
                    p("These graphs display the Oscars Best International Feature Film
                       winners countries over time compared to that of the Cannes Film
                       Festival's top award (the Palme D'Or) and the British Film Academy's
                      equivalent Foreign Film Award."),
                    p("The year range can be adjusted on the left to filter for a
                       certain range."),
                    sliderInput("year_range", "Year:",
                                min = 1946, max = 2020,
                                value = c(1946, 2020),
                                sep = ""),
                    radioButtons(inputId = "year_radio", 
                                 label = "BAFTA & Oscars Films",
                                 choices = list("All" = 1, 
                                                "Nominated Films" = 2, 
                                                "Winning Films" = 3), 
                                 selected = 1)
                ),
             mainPanel(
                plotOutput("oscar_over_time"),
                plotOutput("bafta_over_time"),
                plotOutput("palme_over_time")
             ))
             ),
    tabPanel("Oscar Demographics",
             sidebarLayout(
                 sidebarPanel(
                     h4("About"),
                     p("These graphs display the demographics across race,
                     religion, and sexual orientation over time."),
                     p("The year range can be adjusted on the left to filter for a
                       certain range."),
                     sliderInput("demo_range", "Year:",
                                 min = 1930, max = 2014,
                                 value = c(1930, 2014),
                                 sep = "")
                 ),
                 mainPanel(
                     plotOutput("demographics_over_time")
                 ))
             ),
    tabPanel("Models and Analysis",
        tabsetPanel(
        tabPanel("Gender",
            includeHTML("gender.html")
        ),
        tabPanel("Geography",
            includeHTML("geography.html")
        ),
        tabPanel("IMDb Ratings",
            includeHTML("popularity.html")
        ))
        ),
    tabPanel("About",
             h2("Data"),
             p("I took data from multiple sources. For the demographic data, I
               used data taken from Kaggle - one containing demographic information
               of Oscars award winners, and one with overall Oscars award nominees and
               winners. Most of my data analysis and visualizations
               of demographics here utilized these two datasets."),
             
             p("For my data comparing the various film awards, I scraped it from
               Wikipedia's tables showing the history of these awards for ease. It's
               important to note that the Wikipedia tables references the original sources
               of the data, whether the AMPAS, BAFTA, or Cannes Festival."),
             h2("Motivation"),
             p("My motivation for this project was driven by the news regarding
               the Oscars and my personal passion for movies. Over the years,
               I realized that there was a stereotype that many film awards (and
               the Academy Awards specifically) only honored a certain subset or type
               of movies, and director Bong's comments coming up to the 2020 Oscars
               only confirmed this impression."),
             h2("Contact"),
             
             p("Hello! My name is Richard Zhu, and I'm a sophomore in Leverett 
                House studying Applied Math with a focus in Economics and 
                Computer Science. This is was my final project for GOV 1005: 
                Data. If you're interested in contacting me, my email is richardzhu@college.harvard.edu"),
             p("The source code for this app can be found", a(href = 'https://github.com/richardzhu64/gov1005-final-project', 'here.'))
             ))

server <- function(input, output) {

    output$oscar_countries <- renderPlot({
        # I created a map plot displaying each country's number of submissions to the
        # Oscars' Best International Film Category. I used the rnaturalearth package to
        # get the countries in an sf dataframe, then left_joined it to my biff_countries
        # data. From there, I used ggplot to create a geom_sf() according to the
        # countries data, with the submissions as the fill variable. In the future, I
        # hope to create a Shiny app that gives the options to users about whether they
        # want to see the submissions, winners, or nominations as the fill variable.
        
        countries <- ne_countries(returnclass = "sf") %>%
            clean_names()
        biff_countries_clean <- read_rds("biff_countries_clean.rds") %>%
            mutate(all = winners + nominations + submissions)
        biff_selection <- input$biff_input
        
        biff_countries_world <- countries %>%
            left_join(biff_countries_clean, by=c("name" = "country"))
    
        # create input function that returns certain inputs depending on
        # variable biff_selection
        input_biff <- function(s) {
            if (is.na(s)){
                "all"
            }
            else {
                s
            }
        }
        
        ggplot(biff_countries_world) + 
            geom_sf(aes(fill = !!as.name(tolower(biff_selection)))) +
            scale_fill_viridis_c(option = "plasma",
                                 direction = -1) +
            labs(title = paste("Oscars Best International Film",
                               biff_selection,
                               "by Country"),
                 caption = "Source: Wikipedia/Academy of Motion Picture Arts and Sciences",
                 fill = biff_selection) +
            theme_void()
    })
    output$oscar_over_time <- renderPlot({
        input_low <- input$year_range[1]
        input_high <- input$year_range[2]
        input_radio <- ifelse(input$year_radio == 2, FALSE, TRUE)
        input_name <- case_when(
            input$year_radio == 1 ~ "All",
            input$year_radio == 2 ~ "Nominations",
            input$year_radio == 3 ~ "Winners"
        )
        
        biff_graph_data <- biff_titles %>%
            filter(input$year_radio == 1 | win == input_radio) %>%
            filter(year >= input_low & year <= input_high) %>%
            group_by(country) %>%
            tally() %>%
            arrange(desc(n)) %>%
            left_join(countries, by=c("country"="name")) %>%
            st_as_sf(sf_column_name = "geometry")
        
         biff_graph <- biff_graph_data %>%
            ggplot(text = label) + 
            geom_sf(data = countries) + 
            geom_sf(aes(fill = n)) +
            scale_fill_viridis_c(option = "plasma",
                                 direction = -1) +
            labs(title = "Oscars Best International Film by Country Over Time",
                 caption = "Source: Wikipedia/Academy of Motion Picture Arts and Sciences",
                 fill = paste("BIFF",input_name),
                 subtitle = paste("From", input_low, "to", input_high))
         biff_graph
    })
    output$bafta_over_time <- renderPlot({
        input_low <- input$year_range[1]
        input_high <- input$year_range[2]
        input_radio <- ifelse(input$year_radio == 2, FALSE, TRUE)
        input_name <- case_when(
            input$year_radio == 1 ~ "All",
            input$year_radio == 2 ~ "Nominations",
            input$year_radio == 3 ~ "Winners"
        )
        
        bafta_graph_data <- bafta_year_win %>%
            unnest(country) %>%
            mutate(country = country %>% reverse_countries()) %>%
            filter(input$year_radio == 1 | win == input_radio) %>%
            filter(year >= input_low & year <= input_high) %>%
            group_by(country) %>%
            tally() %>%
            arrange(desc(n)) %>%
            left_join(countries, by=c("country"="name")) %>%
            st_as_sf(sf_column_name = "geometry")
        
        bafta_graph <- bafta_graph_data %>%
            ggplot(text = label) + 
            geom_sf(data = countries) + 
            geom_sf(aes(fill = n)) +
            scale_fill_viridis_c(option = "plasma",
                                 direction = -1) +
            labs(title = "BAFTA Best Foreign Film by Country Over Time",
                 caption = "Source: Wikipedia/BAFTA",
                 fill = paste("BAFTA",input_name),
                 subtitle = paste("From", input_low, "to", input_high))
        bafta_graph
    })
    output$palme_over_time <- renderPlot({
        input_low <- input$year_range[1]
        input_high <- input$year_range[2]
        palme_graph_data <- palme_dor_year_win %>%
            unnest(country) %>%
            mutate(country = country %>% reverse_countries()) %>%
            filter(year >= input_low & year <= input_high) %>%
            group_by(country) %>%
            tally() %>%
            arrange(desc(n)) %>%
            left_join(countries, by=c("country"="name")) %>%
            st_as_sf(sf_column_name = "geometry")
        
        palme_graph <- palme_graph_data %>%
            ggplot(text = label) + 
            geom_sf(data = countries) + 
            geom_sf(aes(fill = n)) +
            scale_fill_viridis_c(option = "plasma",
                                 direction = -1) +
            labs(title = "Cannes Film Festival Palme D'Or Country Over Time",
                 caption = "Source: Wikipedia/Cannes Film Festival",
                 fill = "Palme D'Or Winners",
                 subtitle = paste("From", input_low, "to", input_high))
        palme_graph
    })
    output$demographics_over_time <- renderPlot({
        input_low <- input$demo_range[1]
        input_high <- input$demo_range[2]
        graph_data <- oscar_demographics %>%
            filter(year_of_award >= input_low & year_of_award <= input_high) %>%
            select(person, movie, year_of_award, sexual_orientation, religion,
                   race_ethnicity) %>%
            mutate(race_ethnicity = as.character(race_ethnicity),
                   sexual_orientation = as.character(sexual_orientation),
                   religion = as.character(religion))
        orientation_graph <- graph_data %>%
            filter(!is.na(sexual_orientation)) %>%
            mutate(sexual_orientation = ifelse(sexual_orientation == "Na", "NA",
                                     sexual_orientation)) %>%
            group_by(sexual_orientation) %>%
            tally() %>%
            ggplot(aes(x = reorder(x = sexual_orientation, n), y = n)) +
            geom_bar(stat = "identity", fill = "lightblue") +
            coord_flip() +
            theme_classic() +
            labs(title = "Oscar Winners by \n Sexual Orientation",
                 subtitle = paste("From",input_low, "to", input_high),
                 y = "Sexual Orientation",
                 x = "Count"
            )
        race_graph <- graph_data %>%
            filter(!is.na(race_ethnicity)) %>%
            group_by(race_ethnicity) %>%
            tally() %>%
            ggplot(aes(x = reorder(race_ethnicity, n), y = n)) +
            geom_bar(stat = "identity", fill = "darkgreen") +
            coord_flip() +
            theme_classic() +
            labs(title = "Oscar Winners by \n Race/Ethnicity",
                     subtitle = paste("From",input_low, "to", input_high),
                     y = "",
                     x = "Count"
            )
        religion_graph <- graph_data %>%
                filter(!is.na(religion)) %>%
                mutate(religion = ifelse(religion == "Na", "NA", religion)) %>%
                group_by(religion) %>%
                tally() %>%
                ggplot(aes(x = reorder(religion, n), y = n)) +
                geom_bar(stat = "identity", fill = "red") +
                coord_flip() +
                theme_classic() +
                labs(title = "Oscar Winners by Religion",
                     subtitle = paste("From",input_low, "to", input_high),
                     y = "Religion",
                     x = "Count"
                )
          (race_graph | orientation_graph) / religion_graph +
            plot_layout(height = c(1, 2))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
