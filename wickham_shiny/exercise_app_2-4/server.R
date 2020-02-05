library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    output$product <- renderText({ 
      product <- input$x * input$y
      product
    })
    output$product_plus5 <- renderText({ 
      product <- input$x * input$y
      product + 5
    })
    output$product_plus10 <- renderText({ 
      product <- input$x * input$y
      product + 10
    })
  }
) 