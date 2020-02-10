library(shiny)

# Define server logic 
shinyServer(function(input, output, session) {
    multiple <- reactive({
      input$x * input$y
    })
    output$product <- renderText({ 
      multiple()
    })
    output$product_plus5 <- renderText({ 
      multiple() + 5
    })
    output$product_plus10 <- renderText({ 
      multiple() + 10
    })
  }
) 