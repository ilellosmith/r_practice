library(shiny)
library(tidyverse)
library(shinythemes)
library(bslib)
library(janitor)
library(jsonlite)

#aanalyze_spotify_data <- function(){

  spotify_theme <- bs_theme_update(bs_theme(bootswatch = 'cyborg'),
                                   spacer = '1.25rem',
                                   # Color parameters
                                   bg = '#000000',
                                   fg = '#CECECE',
                                   primary = '#42D760',
                                   'navbar-dark-active-color' = '#42D760',
                                   'navbar-dark-color' = '#e8e8e8',
                                   'body-color' = '#e8e8e8',
                                   'headings-color' = '#e8e8e8',
                                   # font parameters
                                   heading_font = font_google('Roboto'),
                                   'font-size-base' = '1.25rem')

  # Define UI for application that draws a histogram
  ui <- fluidPage(

    theme = spotify_theme,
    #'bootstrap.css',
    #'
    tags$head(
      tags$style(HTML("
        .shiny-output-error-validation {
            color: #d60000;
            font-weight: bold;
        }
        .navbar.navbar-default {
            background-color: #000;
            border: 0px;
        }
        .progress-bar {
            color: #000000;
        }
        .progress {
           font-weight: bold;
        }
    "))),

    # Application title
    titlePanel("Spotify Data Explorer"),

    navbarPage(icon('music', "fa-3x"),
              tabPanel('About', aboutPageUI("aboutTab")),
              tabPanel("Load Files", loadFilesUI("loadFiles")),
              tabPanel("Summary"),
              tabPanel("Your Artists"),
              tabPanel("Your Songs"),
              tabPanel("Your Listening Habits")
    ))

  # Define server logic required to draw a histogram
  server <- function(input, output, session){

      loadFilesServer('loadFiles')

  }

  shinyApp(ui, server)
#}
