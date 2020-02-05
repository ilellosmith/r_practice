# Notes:
# Calling the shiny app:
# to create boilerplate type shinyapp then hit Shift + Tab
# hit cmd + Shift + Enter to launch app
# sourcing the document will run the app
# once you run, will show where it is running Listening on http://127.0.0.1:5221 
# 127.0.0.1 is your computer's address, last 4 digits are randomly assigned port number

# APP 1: 

# library(shiny)
# ui <- fluidPage(
#   selectInput('dataset', label = 'Dataset', choices = ls('package:datasets')),
#   verbatimTextOutput('summary'),
#   tableOutput('table')
# )
# server <- function(input, output, session) {
#   # general output structure in Shiny: 
#   # output$____ <- renderTYPE({
#   #  Expression that generates output for renderTYPE
#   # })
#   
#   # this expression is a reactive expression
#   # * you can call it like a function
#   # * BUT, it only runs the first time it's called and...
#   # ... caches the result until it needs to be updated
#   dataset <- reactive({
#     get(input$dataset, 'package:datasets')
#   })
#   
#   output$summary <- renderPrint({
#     summary(dataset)
#   })
#   
#   output$table <- renderTable({
#     dataset
#   })
# }
# shinyApp(ui, server)

# Exercise APP 2: 

# Greet user by name 
# this prints from the start. Is that right?
library(shiny)

ui <- fluidPage(
  textInput("name", "What's your name?")
  , textOutput('greeting')
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
}

# Exercise APP 3:


shinyApp(ui, server) 



