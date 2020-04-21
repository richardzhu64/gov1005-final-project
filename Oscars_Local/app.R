#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(janitor)
library(purrr)
library(shinythemes)
library(broom)
library(gt)
library(scales)
library(animation)


biff_titles <- readRDS("biff_titles.rds")
bafta_year_win <-  readRDS("bafta_year_win.rds")
palme_dor_year_win <- readRDS("palme_dor_year_win.rds")


# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("sandstone"), "Oscars So Local?: Film Awards by Country and Demographics",
    tabPanel("Home",
             h2("Welcome!", align = "center"),
             h4("International or Local Films?: The Oscars by Country and Demographic Over Time"),
             
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
               membership with its eventual results provides interesting insights
               into whether the representation within membership necessarily 
               causes more diverse winners and nominees. Thus, I utilized online
               data from Kaggle to download demographic information of Oscars 
               nominees historically to see the progress of representation over 
               time.")
             ),
    tabPanel("Oscar Best International Film ",
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
    tabPanel("Oscars Over Time",
             mainPanel(
                 imageOutput("biff_test")
             )
             ),
    navbarMenu("Oscars vs. Film Awards",
        tabPanel("Animations Over Time",
            mainPanel(
                column(4.5, offset = 6,
                       imageOutput("biff_first_animation")),
                hr(),
                fluidRow(
                    column(4.5,
                           imageOutput("biff_animation")
                           ),
                    column(4.5, offset = 6,
                           imageOutput("biff_winners_animation")
                           )
                ),
                hr(),
                fluidRow(
                    column(4.5,
                           imageOutput("bafta_animation")
                    ),
                    column(4.5, offset = 6,
                           imageOutput("bafta_winners_animation")
                    )
                ),
                hr(),
                column(4.5, offset=6,
                       imageOutput("palme_animation"))
            )),
        tabPanel("Interactive Graphs",
             sidebarLayout(
                sidebarPanel(
                    h4("About"),
                    p("These graphs display the Oscars Best International Feature Film
                       winners countries over time compared to that of the Cannes Film
                       Festival's top award (the Palme D'Or) and the British Film Academy's
                      equivalent Foreign Film Award."),
                    p("The year range can be adjusted on the left to filter for a
                       certain range. Hover over each country to get the specific
                       numbers."),
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
                plotOutput("palme_over_time"),
                plotOutput("bafta_over_time")
             )))
             ),
    tabPanel("Oscar Demographics"
             ),
    tabPanel("Data Analysis"
             ),
    tabPanel("About",
             h2("Contact"),
             
             p("Hello! My name is Richard Zhu, and I'm a sophomore in Leverett 
                House studying Applied Math with a focus in Economics and 
                Computer Science. This is was my final project for GOV 1005: 
                Data. If you're interested in contacting me, my email is 
                richardzhu@college.harvard.edu")
             
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
    output$biff_first_animation <- renderImage({
        list(src = "biff_first_time_animation.gif",
             contentType = 'image/gif',
             width = 400
        )}, deleteFile = FALSE)
    output$biff_animation <- renderImage({
            list(src = "biff_over_time_animation.gif",
                 contentType = 'image/gif',
                 width = 400
            )
    }, deleteFile = FALSE)
    
    output$biff_winners_animation <- renderImage({
        list(src = "biff_winners_over_time_animation.gif",
             contentType = 'image/gif',
             width = 400
        )
    }, deleteFile = FALSE)
    output$bafta_animation <- renderImage({
        list(src = "bafta_over_time_animation.gif", 
             width = 400
        )
    }, deleteFile = FALSE)
    
    output$bafta_winners_animation <- renderImage({
        list(src = "bafta_winners_over_time_animation.gif",
             contentType = 'image/gif',
             width = 400
        )
    }, deleteFile = FALSE)
    output$palme_animation <- renderImage({
        list(src = "palme_over_time_animation.gif",
             contentType = 'image/gif',
             width = 400
             
        )
    }, deleteFile = FALSE)
    output$oscar_over_time <- renderPlot({
        input_low <- input$year_range[1]
        input_high <- input$year_range[2]
        input_radio <- ifelse(input$year_radio == 2, FALSE, TRUE)
                         
        if (input_low == 1946 & input_high == 2020) {
            
        }
        else {
            biff_graph <- biff_titles %>%
                filter(input$year_radio == 1 | win == input_radio) %>%
                filter(year >= input_low & year <= input_high) %>%
                group_by(country) %>%
                tally() %>%
                arrange(desc(n))
        }
    })
    output$bafta_over_time <- renderPlot({
        
    })
    output$palme_over_time <- renderPlot({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
