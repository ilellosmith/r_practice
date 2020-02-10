library(shiny)
library(ggplot2)

datasets <- data(package = "ggplot2")$results[, "Item"]

fluidPage(
  selectInput("dataset", "Dataset", choices = datasets),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)