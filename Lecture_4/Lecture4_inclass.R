# This lecture is based on the online tutorial that begins at
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

# Introduction -----

# Shiny is an R package that makes it easy to build interactive web apps straight
# from R.

# The **Hello Shiny** example plots a histogram of R’s faithful dataset with a
# configurable number of bins. 

# Users can change the number of bins with a slider bar, and the app will
# immediately respond to their input. 

library(shiny)

# To run Hello Shiny, type:
runExample("01_hello")

# Structure of a Shiny App -----

# Shiny apps are contained in a single script called `app.R`.

# The script app.R lives in a directory (for example, newdir/) and the app can be
# run with runApp("newdir").

# `app.R` has three components:
# - a user interface (ui) object,
#   controls the layout and appearance of your app.
# - a server function,
#   contains the instructions that your computer needs to build your app.
# - a call to the shinyApp function,
#   creates Shiny app objects from an explicit UI/server pair.

# Note: Prior to version 0.10.2, Shiny did not support single-file apps and
# the ui object and server function needed to be contained in separate scripts
# called ui.R and server.R, respectively. This functionality is still supported
# in Shiny, however the tutorial and much of the supporting documentation focus
# on single-file apps.

# One nice feature about single-file apps is that you can copy and paste the entire
# app into the R console, which makes it easy to quickly share code for others to
# experiment with. For example, if you copy and paste the code above into the R
# command line, it will start a Shiny app.

library(shiny)

# Define UI object ----
ui <- 
  fluidPage(
    # App title
    titlePanel("Hello Shiny!"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        # Input: Slider for the number of bins
        sliderInput(inputId = "bins",
                    label = "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
      ),
      
      # Main panel for displaying outputs
      mainPanel(
        # Output: Histogram
        plotOutput(outputId = "distPlot")
      )
    )
)

# Define server function ----
server <- function(input, output) {
  # Histogram of the Old Faithful Geyser Data
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- 
    renderPlot({
      x    <- faithful$waiting
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x, breaks = bins, col = "#75AADB", border = "white",
           xlab = "Waiting time to next eruption (in mins)",
           main = "Histogram of waiting times")
    })
}


# At one level, the Hello Shiny server function is very simple.
# The script does some calculations and then plots a histogram with
# the requested number of bins.

# However, you'll also notice that most of the script is wrapped in a
# call to renderPlot. We'll cover this concept in much more detail soon.

# Run the app:
shinyApp(ui = ui, server = server)


# Build a user interface -----

# Start with the following script:

library(shiny)
ui <- fluidPage()
server <- function(input, output) { }
shinyApp(ui = ui, server = server)

# This code is the bare minimum needed to create a Shiny app. The
# result is an empty app with a blank user interface.

# Layout ----

# Shiny uses the function `fluidPage` to create a display that automatically
# adjusts to the dimensions of your user's browser window. 

# You lay out the user interface of your app by placing elements in the
# `fluidPage` function.

# For example, the `ui` function below creates a user interface that has a
# title panel and a sidebar layout, which includes a sidebar panel and a
# main panel.

ui <- fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(
    sidebarPanel("sidebar panel"),
    mainPanel("main panel")
  )
)

shinyApp(ui = ui, server = server)

# `titlePanel` and `sidebarLayout` are the two most popular elements to add to
# fluidPage. They create a basic Shiny app with a sidebar.

# `sidebarLayout` always takes two arguments:

# `sidebarPanel` function output
# `mainPanel` function output

# These functions place content in either the sidebar or the main panels.

# The sidebar panel will appear on the left side of your app by default.
# You can move it to the right side by giving `sidebarLayout` the optional
# argument `position = "right"`.

ui <- fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(position = "right",
                sidebarPanel("sidebar panel"),
                mainPanel("main panel")
  )
)

shinyApp(ui = ui, server = server)

# `titlePanel` and `sidebarLayout` create a basic layout for your Shiny app,
# but you can also create more advanced layouts. You can use `navbarPage` to
# give your app a multi-page user interface that includes a navigation bar.
# Or you can use fluidRow and column to build your layout up from a grid system.
# See https://shiny.rstudio.com/articles/layout-guide.html for more details.


# HTML Content -----

# We can add HTML elements, formatted text and images to our panels useing one 
# of Shiny’s HTML tag functions (or no tag for plain text). 
# These functions parallel common HTML5 tags:

# shiny function 	HTML5 equivalent 	creates
# `p` 	          `<p>` 	          A paragraph of text
# `h1`            `<h1>`            A first level header
# `h2` 	          `<h2>`            A second level header
# ...
# `h6` 	          `<h6>` 	          A sixth level header
# `a` 	          `<a>` 	          A hyper link
# `br` 	          `<br>` 	          A line break (e.g. a blank line)
# `div` 	        `<div>` 	        A division of text with a uniform style
# `span` 	        `<span>` 	        An in-line division of text with a uniform style
# `pre` 	        `<pre>` 	        Text ‘as is’ in a fixed width font
# `code` 	        `<code>` 	        A formatted block of code
# `img` 	        `<img>` 	        An image
# `strong` 	      `<strong>` 	      Bold text
# `em`            `<em>` 	          Italicized text
# `HTML` 	  	                      Directly passes a character string as HTML code

# See https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
# for a brief demonstration of these.

# Add control widgets -----

# A widget is a web element that your users can interact with. Widgets provide a
# way for your users to send messages to the Shiny app.

# Shiny widgets collect a value from your user. When a user changes the widget,
# the value will change as well. 

# Shiny comes with a family of pre-built widgets, each created with a
# transparently named R function. For example, Shiny provides a function
# named `actionButton` that creates an Action Button and a function named 
# `sliderInput` that creates a slider bar.

# The standard Shiny widgets are:
# function 	            widget
# --- --- --- --- --- --- --- --- --- --- --- --- 
# `actionButton` 	      Action Button
# `checkboxGroupInput` 	A group of check boxes
# `checkboxInput` 	    A single check box
# `dateInput` 	        A calendar to aid date selection
# `dateRangeInput` 	    A pair of calendars for selecting a date range
# `fileInput` 	        A file upload control wizard
# `helpText` 	          Help text that can be added to an input form
# `numericInput` 	      A field to enter numbers
# `radioButtons` 	      A set of radio buttons
# `selectInput` 	      A box with choices to select from
# `sliderInput` 	      A slider bar
# `submitButton` 	      A submit button
# `textInput` 	        A field to enter text

# Adding widgets

# To add a widget to your app, place a widget function in `sidebarPanel` or
# `mainPanel` in your `ui` object.

# Each widget function requires several arguments:
# The first two arguments for each widget are 
# - a name for the widget: The user will not see this name, but you can
#   use it to access the widget's value. The name should be a character string.
# - a label: This label will appear with the widget in your app. It should be a
#   character string, but it can be an empty string "".

# In the following, the name is "action" and the label is "Action": 
actionButton("action", label = "Action")
# The value is an HTML tag.

# The remaining arguments vary from widget to widget, depending on what the
# widget needs to do its job. They include things like initial values, ranges, 
# and increments. You can find the exact arguments needed by a widget on the
# widget function's help page, (e.g., `?selectInput`).

# The following code demonstrates some basic widgets.
# Play with each widget to get a feel for what it does. 
# Experiment with changing the values of the widget functions and
# observe the effects. 

library(shiny)
ui <- fluidPage(
  titlePanel("Basic widgets"),
  fluidRow(
    column(3,
           h3("Buttons"),
           actionButton("action", "Action"),
           br(),
           br(), 
           submitButton("Submit")),
    column(3,
           h3("Single checkbox"),
           checkboxInput("checkbox", "Choice A", value = TRUE)),
    column(3, 
           checkboxGroupInput("checkGroup", 
                              h3("Checkbox group"), 
                              choices = list("Choice 1" = 1, 
                                             "Choice 2" = 2, 
                                             "Choice 3" = 3),
                              selected = 1)),
    column(3, 
           dateInput("date", 
                     h3("Date input"), 
                     value = "2014-01-01"))   
  ),
  fluidRow(
    column(3,
           dateRangeInput("dates", h3("Date range"))),
    column(3,
           fileInput("file", h3("File input"))),
    column(3, 
           h3("Help text"),
           helpText("Note: help text isn't a true widget,", 
                    "but it provides an easy way to add text to",
                    "accompany other widgets.")),
    column(3, 
           numericInput("num", 
                        h3("Numeric input"), 
                        value = 1))   
  ),
  fluidRow(
    column(3,
           radioButtons("radio", h3("Radio buttons"),
                        choices = list("Choice 1" = 1, "Choice 2" = 2,
                                       "Choice 3" = 3),selected = 1)),
    column(3,
           selectInput("select", h3("Select box"), 
                       choices = list("Choice 1" = 1, "Choice 2" = 2,
                                      "Choice 3" = 3), selected = 1)),
    column(3, 
           sliderInput("slider1", h3("Sliders"),
                       min = 0, max = 100, value = 50),
           sliderInput("slider2", "",
                       min = 0, max = 100, value = c(25, 75))
    ),
    column(3, 
           textInput("text", h3("Text input"), 
                     value = "Enter text..."))   
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)




# Exercise:
# Rewrite your `ui` to create the user interface displayed below. 
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1))
plot(imager::load.image("shiny-gapminder-input.png"), axes = FALSE) 
# Notice that this Shiny app uses a basic Shiny layout (no columns)
# and contains three of the widgets pictured above. 

# Answer starts with:
df <- readRDS("gapminder.rds")


# Display reactive output ----

# Reactive output automatically responds when your user toggles a widget.

# You can create reactive output with a two step process:

# 1. Add an R object to your user interface.
# 2. Tell Shiny how to build the object in the server function. The object will
#    be reactive if the code that builds it calls a widget value.

# Step 1: Add an R object to the UI

# Shiny provides a family of functions that turn **R objects** into 
# **output for user interface**. Each function creates a specific type of output.

# Output function 	  Creates
# --- --- --- --- --- --- --- ---
# dataTableOutput 	  DataTable
# htmlOutput 	        raw HTML
# imageOutput 	      image
# plotOutput 	        plot
# tableOutput 	      table
# textOutput 	        text
# uiOutput 	          raw HTML
# verbatimTextOutput 	text

# Add output to the user interface in the same way that you added HTML elements
# and widgets. 

# Place the output function inside `sidebarPanel` or `mainPanel` in the `ui`.

# For example, the `ui` object below uses `textOutput` to add a reactive line
# of text to the main panel of the Shiny app.

ui <- fluidPage(

  "",
    
    mainPanel(textOutput("selected_continent"))
  )
)

server <- function(input, output) {}

shinyApp(ui, server)


# When we run this app nothing happens. And nothing will happen unless we execute 
# Step 2: Provide R code to build the object.

# Placing a function in `ui` tells Shiny where to display your object. 
# Next, you need to tell Shiny how to build the object.

# We do this by providing the R code that builds the object in the server function.
# The server function plays a special role in the Shiny process; it builds a
# list-like object named output that contains all of the code needed to update
# the R objects in your app. Each R object needs to have its own entry in the list.

# You can create an entry by defining a new element for output within the server
# function, like below. The element name should match the name of the reactive
# element that you created in the `ui`.

# In the server function below, output$selected_var matches textOutput("selected_var")
# in your ui:

server <- function(input, output) {
  output$selected_continent <- renderText({ 
    "You have selected Atlantis"
  })
}

shinyApp(ui, server)

# You do not need to explicitly state in the server function to return output
# in its last line of code.

# **Each entry to output** should contain the output of one of Shiny’s `render*`
# functions. These functions capture an R expression and do some light
# pre-processing on the expression. 

# Use the render* function that corrresponds to the type of reactive object you
# are making.

# render function 	creates
# --- --- --- --- --- --- --
# renderDataTable 	DataTable
# renderImage 	    images (saved as a link to a source file)
# renderPlot 	      plots
# renderPrint 	    any printed output
# renderTable 	    data frame, matrix, other table like structures
# renderText 	      character strings
# renderUI 	        a Shiny tag object or HTML

# Each `render*` function takes a single argument: an R expression surrounded by
# braces, `{}`. The expression can be one simple line of text, or it can involve
# many lines of code, as if it were a complicated function call.

# Think of this R expression as a set of instructions that you give Shiny to store
# for later. Shiny will run the instructions when you first launch your app, and
# then Shiny will re-run the instructions every time it needs to update your object.

# For this to work, your expression should return the object you have in mind
# (a piece of text, a plot, a data frame, etc.). You will get an error if the
# expression does not return an object, or if it returns the wrong type of object.

# Use widget values -----

# You can make the text reactive by asking Shiny to call a widget value when it
# builds the text.

# `input` is a second list-like object. It stores the current values of all of
# the widgets in your app. These values will be saved under the names that you
# gave the widgets in your `ui`.

# Shiny will automatically make an object reactive if the object uses an input
# value. For example, the server function below creates a reactive line of text by
# calling the value of the select box widget to build the text.

server <- function(input, output) {
  
  output$selected_continent <- renderText({ 
    paste("You have selected", paste(input$continent, collapse = ", "))
  })
  
}

shinyApp(ui, server)

# Exercise:
# Create an app whose input is a slider that allows a non negative range of integers
# between -100 and 100. The output in the main panel should be a plot of `x^2` where
# `x` is a sequence of numbers between the minimal and maximal values of the range.

# https://sta697v.shinyapps.io/x2app/

# Loading files and file paths -----

# To use the gapminder dataset we loaded `gapminder.rds` with the `readRDS` function. 
# We will also need `library(ggplot2)` and `library(dplyr)`.

library(ggplot2)
library(dplyr)
df <- readRDS("gapminder.rds")

# `readRDS` requires a file path, and file paths do not
# behave the same way in a Shiny app as they do at the command line.

# When Shiny runs the commands in `app.R`, it will treat all file paths as
# if they begin in the same directory as `app.R`. In other words, the directory
# that you save `app.R` in will become the working directory of your Shiny app
# (even without defining an RStudio project)

plot(imager::load.image("run-once.png"), axes = FALSE) 
plot(imager::load.image("run-once-per-user.png"), axes = FALSE) 
plot(imager::load.image("run-many-times.png"), axes = FALSE) 

# - The `shinyApp` function is run once, when you launch your app
# - The `server` function is run once each time a user visits your app
# - The R expressions inside `render*` functions are run many times. 
#   Shiny runs them once each time a user change the value of a widget.

# Source scripts, load libraries, and read data sets at the beginning of
# `app.R` outside of the server function. Shiny will only run this code once,
# which is all you need to set your server up to run the R expressions
# contained in server.

# Define user specific objects inside server function, but outside of any
# `render*` calls. These would be objects that you think each user will need
# their own personal copy of. For example, an object that records the user's
# session information. This code will be run once per user.

# Only place code that Shiny must rerun to build an object inside of a `render*`
# function. Shiny will rerun all of the code in a `render*` chunk each time a user
# changes a widget mentioned in the chunk. This can be quite often.

# You should generally avoid placing code inside a `render` function that does
# not need to be there. Doing so will slow down the entire app.

# Exercise:
# Complete our gappminder app so that it uses ggplot to show a similar plot to
# the familiar gapminder plot (see introduction)
# 1. Data is filtered according to the continent
# 2. and then according to quantiles of interest in income (so that if we only choose
#    Africa, and the range is 0-50, we'll see the lower half of African countries)

# See:
# https://sta697v.shinyapps.io/Gappminder/