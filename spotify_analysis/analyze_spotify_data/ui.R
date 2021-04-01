#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinythemes)
library(bslib)



spotify_theme <- bs_theme_update(bs_theme(bootswatch = 'cyborg'), 
                spacer = '1.25rem',
                # Color parameters
                bg = '#000000', 
                fg = '#CECECE', 
                primary = '#42D760', 
                'navbar-dark-active-color' = '#42D760',
                'navbar-dark-color' = '#e8e8e8', 
                'navbar-toggler-border-radius' = '0rem',
                'nav-tabs-border-width' = '0rem',
                'nav-pills-border-radius' = '0rem',
                'border-radius' = '0rem',
                'border-color' = '0rem', 
                'navbar-dark-toggler-border-color' = '#000000',
                'navbar-light-toggler-border-color' = '#000000',
                'body-color' = '#e8e8e8', 
                'headings-color' = '#e8e8e8',
                # font parameters 
                 heading_font = font_google('Roboto'), 
                 'font-size-base' = '1.25rem')
                # 'nav-tabs-borde r-radius' = 0,
                # 'nav-tabs-border-color' = '#000000',
                # 'navbar-toggler-border-radius' = 0, 
                # 'navbar-light-toggler-border-color' = '#000000', 
                # 'navbar-dark-toggler-border-color' = '#000000')
                #,
               # nav-tabs-link-active-color = "#42D760")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    theme = spotify_theme,
        #'bootstrap.css',

    # Application title
    titlePanel("Spotify Data Explorer"),
    
    navbarPage(icon('music', "fa-3x"),
               tabPanel("About", 
                        tags$style(type = 'text/css', 
                            ".nav {
                            border: 10px;
                            border-color: #000000;
                            border-width: 0rem;
                            font-size: 23px;
                            font-weight: bold;
                            }"
                        ),
                        fluidRow(
                           column(12,
                            p('I recently downloaded and analyzed my Spotify data 
                              from the past year, and built 
                              this R Shiny app so you could do the same with your data.
                              Follow the steps below to download your data from Spotify. Then
                              upload it to the app, put on a playlist 
                              and enjoy!')
                        )), 
                        fluidRow(
                            column(8, 
                                 h3('How to use this app'), 
                                    # Requesting Data
                                     h5('1. Request a copy of your data'),
                                     tags$ol(
                                         tags$li(a(href = 'https://www.spotify.com/us/account/privacy/', 'Head to your Spotify privacy settings')),
                                         tags$li(('Scroll to the bottom of the page, and follow Spotify\'s steps to download your data')),
                                         br(),
                                         img(src = 'spotify_download.png', width = '707', height = '632'),
                                         br(), 
                                         br(),
                                         tags$li(p('Spotify (noreply@spotify.com) will send you an email asking you to confirm your 
                                         data request. Once you\'ve confirmed, Spotify will
                                         email you a link to download your data. In my experience, this takes them 
                                         1-4 days.')),
                                         img(src = 'spotify_emails.png', width = '1097', height = '108'),
                                         br(), 
                                         br(),
                                     type = 'a'),
                                    # Analyzing Data
                                     h5('2. Analyze your data'),
                                         p(HTML('Once you have your data downloaded, follow the directions in the <b>Load Files</b> 
                                         tab above to load your data to the app. Then choose your own adventure exploring the remaining tabs:'
                                         )),
                                         tags$ul(
                                             tags$li(HTML('<b>Summary</b> - an overview of your Spotify activity from the past year')),
                                             tags$li(HTML('<b>Your Artists</b> - a dive into your top artists')),
                                             tags$li(HTML('<b>Your Songs</b> - a look at your top songs')),
                                             tags$li(HTML('<b>Your Listening Habits</b> - info on when and how you listened'))
                                        )
                            )
                        )
                    ),
               tabPanel("Load Files"),
               tabPanel("Summary"),
               tabPanel("Your Artists"),
               tabPanel("Your Songs"),
               tabPanel("Your Listening Habits")
)))
