# Exercise from https://mastering-shiny.org/basic-app.html#exercises
# Exercise 1: Greet user by name 
# to run, set wd to parent folder and run runApp() in console. Or, run 
# runApp(path to parent folder)
library(shiny)

ui <- fluidPage(
  textInput("name", "What's your name?"), 
  textOutput('greeting')
)



