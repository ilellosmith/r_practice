#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to multiply a user input integer by 5
shinyServer(function(input, output) {

    output$multiple <- renderText({
        multiple <- paste0('Then, x multiplied by 5 is: ', input$x*5)
    })

})
