# Exercise 3: Multi slider bar multiplier

library(shiny)

# Define UI for application that multiplies two user inputs 
shinyUI(fluidPage(

    # Application title
    titlePanel("Multiply x by y"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("x",
                        "If x is:",
                        min = 1,
                        max = 50,
                        value = 30), 
            sliderInput("y",
                        "And y is:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput(outputId = 'multiple')
        )
    )
))
