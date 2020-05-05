library(shiny)
runExample("01_hello")      # a histogram
runExample("02_text")       # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg")        # global variables
runExample("05_sliders")    # slider bars
runExample("06_tabsets")    # tabbed panels
runExample("07_widgets")    # help text and submit buttons
runExample("08_html")       # Shiny app built from HTML
runExample("09_upload")     # file upload wizard
runExample("10_download")   # file download wizard
runExample("11_timer")      # an automated timer

# Notes:

# runApp('/path_to_my_app/sub_folder/', "my_app")
# ^ runApps first argument is the path to the application folder. Default is 
# working directory

# runApp("App-1", display.mode = "showcase")
# ^ runApp also has an argument - display.mode that you can set to "showcase"
# which will show the underlying code for the app. Default is "normal" in which
# case this is hidden

# cmd + shift + enter runs a shiny app
# you can run the app in viewer pane within R. Just go to the Run App button 
# on the top and hit the dropdown
