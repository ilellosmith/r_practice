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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    load_data <- function(files){
        files <- input$files
        ext <- tools::file_ext(files$datapath)
        
        req(files)
        shiny::validate(need(ext == "json", "Please upload a .json file"))
        
        files$datapath %>% 
            map_df(~jsonlite::fromJSON(.)) %>%
            #jsonlite::fromJSON() %>% 
            as_tibble() %>% 
            janitor::clean_names()
        
            # map_df(~jsonlite::fromJSON(.)) %>% 
            # as_tibble() %>% 
            # janitor::clean_names()
    }
    
    #reactive({dat <- load_data(input$files)})
    
    output$dat <- renderTable(dim(load_data(input$files)))
})

