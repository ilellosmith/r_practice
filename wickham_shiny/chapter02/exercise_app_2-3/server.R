library(shiny)

# Define server logic required to multiply two input integers
shinyServer(function(input, output) {

    output$multiple <- renderText({
        multiple <- paste0('Then, x multiplied by y is: ', input$x*input$y)
    })

})
