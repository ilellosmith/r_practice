#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(janitor)
library(jsonlite)
library(fastdigest)

vdigest <- Vectorize(fastdigest)

# Define server logic required to draw a histogram
shinyServer(function(input, output){

#   load_data_server <- function(id){
#     moduleServer(id, function(input, output, session) {
#       load_data <- function(file_uploads){
#         
#         # Get uploaded files
#         files <- input$file_uploads
#         
#         # Check files uploaded before QAing input
#         req(files)
#         
#         # QA Input
#         # Check files are JSON
#         ext <- tools::file_ext(files$datapath)
#         shiny::validate(need(ext == "json", "It looks like you uploaded one or more non-json files. Please upload only .json files"))
#         
#         # Check filenames have StreamingHistory in the name
#         names <- str_to_lower(files$name)
#         all_streaming_history <- all(str_detect(names, 'streaminghistory'))
#         shiny::validate(need(all_streaming_history, 'It looks like you uploaded one or more files that do not contain streaming history. Please only upload files with "StreamingHistory" in the name.'))
#         
#         # If so, load and dedupe
#         loaded <- files$datapath %>% 
#           map_df(~jsonlite::fromJSON(.)) %>%
#           as_tibble() %>% 
#           janitor::clean_names()
#         
#         expected_cols <- c('end_time', 'artist_name', 'track_name', 'ms_played')
#         
#         #  len(colnames(loaded)) == len(expected_cols) & all(colnames(loaded) == expected_cols)
#         
#         shiny::validate(need(all(expected_cols %in% colnames(loaded)), 'The columns in the loaded data do not contain the names this app is designed to handle. The app needs each of: endTime, artistName, trackName and msPlayed as property names in the StreamingHistory json files. If you open one of the json files on your desktop, it should look like: 
#   {
#     "endTime" : "2020-04-01 20:56",
#     "artistName" : "Some artist name here",
#     "trackName" : "Some track name here",
#     "msPlayed" : 189803
#   }
# If this is not the case, Spotify likely changed their streaming history file format. Please contact me for updates to the app.'))
#         
#         # dedupe
#         loaded %>% 
#           group_by(end_time, artist_name, track_name) %>%
#           mutate(id = cur_group_id()) %>%
#           arrange(id, desc(ms_played)) %>%
#           slice(1) %>%
#           ungroup()
#         
#       }
#       uploaded_dat <- reactive({load_data(input$file_uploads)})
#       output$dat <- renderTable(dim(uploaded_dat()))
#   })
#   }

  load_data <- function(file_uploads){
    
    # Get uploaded files
    files <- input$file_uploads
    
    # Check files uploaded before QAing input
    req(files)
    
    # QA Input
    # Check files are JSON
    ext <- tools::file_ext(files$datapath)
    shiny::validate(need(ext == "json", "It looks like you uploaded one or more non-json files. Please upload only .json files"))
    
    # Check filenames have StreamingHistory in the name
    names <- str_to_lower(files$name)
    all_streaming_history <- all(str_detect(names, 'streaminghistory'))
    shiny::validate(need(all_streaming_history, 'It looks like you uploaded one or more files that do not contain streaming history. Please only upload files with "StreamingHistory" in the name.'))
    
    # If so, load and dedupe
    loaded <- files$datapath %>% 
      map_df(~jsonlite::fromJSON(.)) %>%
      as_tibble() %>% 
      janitor::clean_names()
    
    expected_cols <- c('end_time', 'artist_name', 'track_name', 'ms_played')
    
    #  len(colnames(loaded)) == len(expected_cols) & all(colnames(loaded) == expected_cols)
    
    shiny::validate(need(all(expected_cols %in% colnames(loaded)), 'The columns in the loaded data do not contain the names this app is designed to handle. The app needs each of: endTime, artistName, trackName and msPlayed as property names in the StreamingHistory json files. If you open one of the json files on your desktop, it should look like: 
  {
    "endTime" : "2020-04-01 20:56",
    "artistName" : "Some artist name here",
    "trackName" : "Some track name here",
    "msPlayed" : 189803
  }
If this is not the case, Spotify likely changed their streaming history file format. Please contact me for updates to the app.'))
    
    # dedupe
    loaded %>% 
      group_by(end_time, artist_name, track_name) %>%
      mutate(id = cur_group_id()) %>%
      arrange(id, desc(ms_played)) %>%
      slice(1) %>%
      ungroup()
    
  }
  uploaded_dat <- reactive({load_data(input$file_uploads)})
  output$dat <- renderTable(dim(uploaded_dat()))
  
    #output$dat <- renderTable(dim(load_data(input$files)))
    
})

