library(shiny)

# input functions get values from user to pass to the backend 
# e.g. selectInput(), sliderInput()
# Input function arguments: 
# 1) all input functions have the same first argument: 
#   inputId
#   must be unique!
#   connects the front end (ui) to back end (server)
# 2) most have a second parameter which is the label
#   this should be a human readable label for the GUI
# 3) third is usually a default value for the input

# inputs you'll likely use for media tool 
# dateRangeInput - lets you input a range of dates
# selectInput - lets you drop down to select different options
# fileInput - lets a user upload a file