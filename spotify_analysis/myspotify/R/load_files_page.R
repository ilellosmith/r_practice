# -----------------------------------------------------------------------------

# UI and Server for Load Files page

# -----------------------------------------------------------------------------

loadFilesUI <- function(id){

  ns <- NS(id)

  fluidPage(
      # Guidance for downloading and uploading files
      fluidRow(
        column(12,
               p(HTML(
                 "Once you have your data back from Spotify,
                                         download and open the .zip file. It's worth reading
                                         through their <a href = 'https://support.spotify.com/us/article/understanding-my-data/'> article </a>
                                         documenting what's in each file they sent you. The most important detail here is that you
                                         should <b> only upload your streaming history file(s) </b> to this app. These are named StreamingHistory#.json,
                                         and look like:"
               )),
               img(src = 'streaming_history_files.png', width = '246', height = '119'),
               br(),
               br(),
               p(HTML(
                 "The Spotify download also includes your profile information and payment
                                         information. Please <i> do not </i> upload these. When you're ready,
                                         hit the browse button below, navigate to your Spotify data download folder,
                                         and select all the streaming history files you want to analyze.
                                         To select multiple files on a Mac, hold down the command button ⌘
                                         and click on each file you want to upload.
                                         Or select the first streaming history file, hold down shift and select the last streaming history
                                         file to select both and all files in between. This second option will also work on Windows."
               ))
        )),
      # File input section
      fluidRow(
        column(12,
               br(),
               fileInput(ns("file_uploads"), "Upload Streaming History File(s)",
                         multiple = TRUE,
                         accept = c('.json'),
                         placeholder = 'No file selected',
                         width = '800px'),
               br(),
               #tableOutput(ns("dat")),
               htmlOutput(ns('timing_text')), # show conditionally if they've hit browse button
               br(),
               br()
      )
  ))
}

loadFilesServer <- function(id) {

  #stopifnot(is.reactive(file_uploads))

  moduleServer(id, function(input, output, session) {

    #' Verifies file name passes tests and loads files
    #' @param file_uploads a reactive object containing input from fileInput
    #' @returns tibble of loaded data
    #'
    load_data <- function(file_uploads){

      # Get uploaded files
      files <- file_uploads

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

    # Load data
    uploaded_dat <- reactive({load_data(input$file_uploads)})

    # Return data
    output$dat <- reactive({uploaded_dat()})

    # Output min and max date for user QA
    min_max <- reactive({

      dates <- uploaded_dat() %>%
      select(end_time) %>%
      transmute(end_date = lubridate::date(end_time))

      list(min(dates$end_date, na.rm = T),
           max(dates$end_date, na.rm = T))
      })

    output$timing_text <- renderText({
      paste0('The data you uploaded covers your listening history from ', min_max()[[1]],
             ' to ', min_max()[[2]], '. If this sounds complete, feel free to move onto the data
                                exploration tabs. Otherwise, hit Browse to retry your load with more
                                data files.')
  })

    }
  )
}
