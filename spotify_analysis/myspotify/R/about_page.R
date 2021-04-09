# -----------------------------------------------------------------------------

# UI and Server for About page

# -----------------------------------------------------------------------------

aboutPageUI <- function(id){
  ns <- NS(id)

  fluidPage(
    # Styling
    tags$style(type = 'text/css',
                            ".nav {
                            border: 10px;
                            border-color: #000000;
                            border-width: 0rem;
                            font-size: 23px;
                            font-weight: bold;
                            }"
  ),
  # Introductory Text
  fluidRow(
    column(12,
           p('I recently downloaded and analyzed my Spotify data
                              from the past year, and built
                              this R Shiny app so you could do the same with your data.
                              Follow the steps below to download your data from Spotify. Then
                              upload it to the app, put on a playlist
                              and enjoy!')
    )),
  # Instructions for App Use
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
           )),
          br())
  )

}

aboutPageServer <- function(id){}
