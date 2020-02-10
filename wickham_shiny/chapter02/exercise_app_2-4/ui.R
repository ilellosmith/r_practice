# Exercise 4: 
# Replace the UI and server components of your app from the previous exercise
# with the UI and server components below, run the app, and describe the app’s
# functionality. Then reduce the duplication in the app by using a reactive expression.

library(shiny)

fluidPage(
    sliderInput("x", "If x is", min = 1, max = 50, value = 30),
    sliderInput("y", "and y is", min = 1, max = 50, value = 5),
    "then, (x * y) is", textOutput("product"),
    "and, (x * y) + 5 is", textOutput("product_plus5"),
    "and (x * y) + 10 is", textOutput("product_plus10")
)