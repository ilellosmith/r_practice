library(shiny)
library(lubridate)

ui <- fluidPage(
 sliderInput('date', 'When should we deliver?',
             value = as_date('2019-08-10'),
             min = as_date('2019-08-09') ,
             max = as_date('2019-08-16'))
)


