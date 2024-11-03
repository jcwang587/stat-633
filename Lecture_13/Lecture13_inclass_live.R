# Reactivity in shiny -----
# Based on: 
# https://vimeo.com/rstudioinc/review/131218530/212d8a5a7a/

# We've seen input$x -> output$y, that changes automatically.

# But we can do much more:
# Use inputs to run code without output.
# Use inputs to modify other reactive objects which will in turn effect outputs.
# Do the above with an "Update" button (instead of automatically)


# Reactive values -----

# Reactive value is a value that reacts to change in some sort of input. like
sliderInput(inputId = "num", 
            label = "Choose a number", 
            value = 25, min = 1, max = 100)
# Changes without explicit code that modifies them.

# Reactive functions -----

# Reactive values, the values of the input, are available on the server side.
# e.g. we can access the above slider input reactive values via `input$num`.
# Reactive values always work with a reactive function - a function that knows
# how to take a reactive value, and knows what to do with it. E.g. `renderPlot()`

# Example (no reactive function):
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist <- 
    hist(rnorm(input$num))
}

shinyApp(ui = ui, server = server)
# We get an error message.

# Exercise:
# Correct the above by adding a reactive function.

# Answer (modify the following):
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist <-
    renderPlot({
      hist(rnorm(input$num))
    })
}

shinyApp(ui = ui, server = server)

# There are additional reactive functions aside of `render*()` functions.

# The output object created by a reactive function is called an "observer". The observer
# and the reactive value create reactivity in the app in the following fashion:

# 1. Reactive values notify the objects that depend on them that they have changed.
#    It's job is done.
# 2. the observer object runs code that looks up the reactive value.

# We will focus today on 7 essential reactive functions and what they can do.

# Always ask yourself:
# What code will the function actually use to re/build the objects?
# Which reactive values the object will respond to?

# 1. render* functions -----

# We've met those! Some of the most popular are:
# render function 	creates
# --- --- --- --- --- --- --
# renderDataTable 	DataTable
# renderImage 	    images (saved as a link to a source file)
# renderPlot 	      plots
# renderPrint 	    any printed output
# renderTable 	    data frame, matrix, other table like structures
# renderText 	      character strings
# renderUI 	        a Shiny tag object or HTML
# renderPlotly      a plotly object

# In addition, we can use renderPlotly to correctly display 
# plotly plots.

# These functions build something that can actually be displayed. 

# Basic syntax:
# renderPlot( { hist(rnorm(input$num)) })

# The code within the curly braces (can be long) is passed to the
# object that the render function makes.
# The object responds **every time** any reactive value in the code
# changes. I.e., every line of code within the chunk will be rerun when
# a reactive value changes.


# Exercise:
# Translate the above app to do the histogram with plotly.

# Answer:
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  plotlyOutput("hist")
)

server <- function(input, output) {
  output$hist <-
    renderPlotly({
      plot_ly() %>%
        add_histogram(x = rnorm(input$num))
    })
}

shinyApp(ui = ui, server = server)


# Example: two inputs -----
ui <- fluidPage(
  sliderInput(
    inputId = "num", 
    label = "Choose a number", 
    value = 25, min = 1, max = 100
    ),
  textInput(
    inputId = "title", 
    label = "Write a title",
    value = "Histogram of Random Normal Values"
    ),
  plotOutput("hist")
)


server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num), main = input$title)
  })
}

shinyApp(ui = ui, server = server)

# Whenever we change, say, input$title, renderPlot runs the code 
# within the curly braces. In the process, it updates all the reactive
# values that are in it, including input$num.


# Exercise:
# Add the title to the plotly app from the previous exercise:

# Answer:
ui <- fluidPage(
  sliderInput(
    inputId = "num", 
    label = "Choose a number", 
    value = 25, min = 1, max = 100
  ),
  textInput(
    inputId = "title", 
    label = "Write a title",
    value = "Histogram of Random Normal Values"
  ),
  plotlyOutput("hist")
)

server <- function(input, output) {
  output$hist <-
    renderPlotly({
      plot_ly() %>%
        add_histogram(x = rnorm(input$num)) %>%
        layout(title = input$title)
    })
}

shinyApp(ui = ui, server = server)



# 2. reactive() -----

# Example: two outputs, single input -----
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  plotOutput("hist"),
  verbatimTextOutput("stats")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
  output$stats <- renderPrint({
    summary(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)

# We create a histogram and show summary statistics of a normal
# random sample where the input slider determines the sample size.
# For this, we use two render* functions: renderPlot and renderPrint.

# When the reactive value changes, both objects are notified. 
# First the histogram will run, then the stats object will run.

# In this example each object is creating its own data to describe. 
# What if we want to have both output objects describing the same data set?

# One way to think about this is to create an intermediary step that generates
# the data once, and sends it to both observers.

par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1))
plot(imager::load.image("Lecture13_fig1.png"), axes = FALSE)
plot(imager::load.image("Lecture13_fig2.png"), axes = FALSE)

# The `reactive()` function builds a reactive object,
# aka "reactive expression"

# Syntax:
# data <- reactive( { rnorm(input$num) } )

# As with the render* functions, the code will run once whenever an
# associated reactive value changes, and re/builds an object.
# The object can be objects like data frames, lists, scalar values,
# or graph objects.

# The reactive expressions are FUNCTIONS. That is, if you create a
# reactive expression names `data`, you have to call it as `data()`
# (without arguments).

# Example: reactive()
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  plotOutput("hist"),
  verbatimTextOutput("stats")
)

server <- function(input, output) {
  
  data <- reactive({
    # the last row of code, will be the value that is considered to be the
    # output of the `reactive function`.
    print(input$num)
    rnorm(input$num)
  })
  
  output$hist <- renderPlot({
    hist(data()) # do "" here
  })
  output$stats <- renderPrint({
    summary(data())  # do "" here
  })
}

shinyApp(ui = ui, server = server)

# Reactive expressions are special: they "know" whether they are 
# valid or invalid. 
# I.e. if a reactive value associated with it hasn't changed,
# the reactive expression, when called, say, from 
# a render* function, will not update itself based in the input
# unless the input has changed.
# Reactive expression also cache their values and send those
# whenever they are called and valid.

# In the above example's workflow, whenever input$num changes, the 
# reactive expression data() is updated and then sends the value to
# the histogram. After the histogram has run, stats will run. But because
# the reactive expression is valid, it won't rerun its code, and 
# send the same data to stats.

# Exercise:
# Create a similar app based on a plotly histogram.

# Answer:
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  plotlyOutput("hist"),
  verbatimTextOutput("stats")
)

server <- function(input, output) {
  
  data <- reactive({
    # the last row of code, will be the value that is considered to be the
    # output of the `reactive function`.
    print(input$num)
    rnorm(input$num)
  })
  
  output$hist <- renderPlotly({
    d <- data()
    plot_ly() %>%
      add_histogram(x = d)
  })
  output$stats <- renderPrint({
    summary(data())  # do "" here
  })
}

shinyApp(ui = ui, server = server)


# 3. isolate() -----

# Example: redundant reactivity -----

# Going back to the two inputs example:
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num), main = input$title)
  })
}

shinyApp(ui = ui, server = server)

# If we type the title a little slow, we get too many updates.

# We want, e.g., to stop the app from responding to the title field,
# but still allow it to change when the slider changes.

# Syntax:
# isolate( { rnorm(input$num) } )

# isolate creates output that is not reactive.
# Similarly to the above functions, it takes a block of code as input.



# Example: isolate -----
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num), main = isolate(input$title))
  })
}

shinyApp(ui = ui, server = server)

# So now this output uses title, but does not react to it.
# When we change input$num, it will also look for the value
# of input$title.

# Exercise:
# Add the same functionality to the plotly app.

# Answer:





# 4. observeEvent() / observe() -----

# When we want to wait for a specific action to be taken
# from the user, like clicking an actionButton(), before
# calculating an expression or taking an action we Use
# `observeEvent()`

# Potentially, this is code that the user will never see -
# save/download file, connect to database, or other ideas.

# Syntax:
# observeEvent( input$clicks, { print(input$clicks) } )

# The first argument is either a reactive value or a vector of
# reactive values - the values that observeEvent tracks.
# The second argument is code to run whenever the reactive value/s
# in the first argument changes.

# The reactive values within the code are not really reactive, 
# think of those as if they were inside `isolate()`.
# `observeEvent` generates R values (not reactive).

# Example: actionButton / observeEvent -----
ui <- fluidPage(
  actionButton(inputId = "clicks", 
               label = "Click me"),
  actionButton(inputId = "clicks2", 
               label = "Click me too")
)

server <- function(input, output) {
  observeEvent(input$clicks, {
    print(
      paste0("Button1: ", input$clicks,
             "; Button2: ", input$clicks2))
  })
}

shinyApp(ui = ui, server = server)

# We see that the R values that are printed in the console reflect
# the number of clicks on the action button.

# (in general, the app should not depend on the actual value of 
# actionButtons).

# 5. observe() -----

# observe also triggers code to run on the server.

# Syntax:
# observe( { print(input$clicks) } )

# The argument is code that is run **every time any reactive value
# in the code changes**.

# Example: actionButton / observe -----
ui <- fluidPage(
  actionButton(inputId = "clicks", 
               label = "Click me"),
  actionButton(inputId = "clicks2", 
               label = "Click me too")
)

server <- function(input, output) {
  observe({
    print(
      paste0("Button1: ", input$clicks,
             "; Button2: ", input$clicks2))
  })
}

shinyApp(ui = ui, server = server)

# 6. eventReactive() -----

# Example task: have a plot updated only after an update button
# was clicked.


# Syntax:
# data <- eventReactive(input$go, { rnorm(input$num) } )

# Similar to observeEvent - the first argument is a reactive value
# or a vector of reactive values to which eventReatcive will respond.
# The second argument is code that will be used to re/build the object.

# inputs that appear in the second argument will be as is `isolate`d 
# will not update unless explicitly called in the first argument.

# Example: eventReactive -----
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  actionButton(inputId = "go", 
               label = "Update"),
  plotOutput("hist")
)

server <- function(input, output) {
  data <- 
    eventReactive(
      input$go, 
      { rnorm(input$num) }
    )
  
  output$hist <- renderPlot({
    hist(data())
  })
}

shinyApp(ui = ui, server = server)

# We might want to also update the histogram on upload.
# To do this we can add an `id` option to `fluidPage`
# which will create a reactive value that is updated on 
# loading the page.

plot(imager::load.image("Lecture13_fig3.png"), axes = FALSE)
plot(imager::load.image("Lecture13_fig4.png"), axes = FALSE)


# Exercise:
# update the histogram once on loading the app:
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  actionButton(inputId = "go", 
               label = "Update"),
  plotOutput("hist"),
  id = "fluid_page" # Hint: something needs to be added/changed here
)

server <- function(input, output) {
  data <- 
    eventReactive(
      list(input$go, input$fluid_page),    # Hint: something needs to be added/changed here
      { 
        # print(input$fluid_page)
        rnorm(input$num) 
        }
    )
  
  output$hist <- renderPlot({
    hist(data())
  })
}

shinyApp(ui = ui, server = server)


# 7. reactiveValues() -----

# shiny does not allow us to override the input values from the user.

# `reactiveValues` allows us to generate reactive values that we can
# change programmatically. 

# Syntax:
# rv <- reactiveValues( data = rnorm(100) )

# The arguments are items to add to the `input` list.

# Example: reactiveValues -----

# Let us use two action buttons to assign two types of 
# data to a reactive value plot
# a histogram of this reactive value.

ui <- fluidPage(
  actionButton(inputId = "norm", label = "Normal"),
  actionButton(inputId = "unif", label = "Uniform"),
  plotOutput("hist")
)

server <- function(input, output) {
  
  rv <- reactiveValues(data = rnorm(100))
  
  observeEvent(input$norm, { rv$data <- rnorm(100) })
  observeEvent( input$unif, { rv$data <- runif(100) })
  
  output$hist <- renderPlot({ 
    hist(rv$data)
  })
}

shinyApp(ui = ui, server = server)

plot(imager::load.image("Lecture13_fig5.png"), axes = FALSE)

# Exercise:
# Make a plotly version of the above app.

# Answer:
ui <- fluidPage(
  actionButton(inputId = "norm", label = "Normal"),
  actionButton(inputId = "unif", label = "Uniform"),
  plotlyOutput("hist")
)

server <- function(input, output) {
  
  rv <- reactiveValues(data = rnorm(100))
  
  observeEvent(input$norm, { rv$data <- rnorm(100) })
  observeEvent( input$unif, { rv$data <- runif(100) })
  
  output$hist <- renderPlotly({ 
    plot_ly() %>% add_histogram(x = rv$data)
  })
}

shinyApp(ui = ui, server = server)


plot(imager::load.image("Lecture13_fig5.png"), axes = FALSE)



# Summary -----

plot(imager::load.image("Lecture13_fig6.png"), axes = FALSE)


# Exercise:
# Start from the two inputs example with plotly.
# Add an action button that will update the histogram's title (so
# that the actual title does not change unless the update button is
# clicked). In addition, modify the app such that every
# time you change the title a random color is chosen for the bars
# (you can simply choose an integer between 1 to 10 to represent a color).
# In addition, make sure that the numerical values for the histogram
# are only updated when the slider moves (not when the update button
# is clicked). 


# Start from:
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  plotlyOutput("hist")
)

server <- function(input, output) {
  output$hist <- renderPlotly({
    plot_ly(data.frame(x = rnorm(input$num))) %>%
      add_histogram(x = ~x) %>%
      layout(title = input$title)
  })
}

shinyApp(ui = ui, server = server)

# Answer:






# Update inputs -----

# shiny also allows us to update the parameters of input widgets
# based on the values of other input widgets.

# update*() functions combined with a reactive function will
# allow us to do so:

# Example: updateInputSlider -----
ui <- 
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        p("The first slider controls the range of the second"),
        sliderInput(inputId = "control",
                    label = "Controller:",
                    min = 0, max = 20, value = 10,
                    step = 1),
        sliderInput(inputId = "receive",
                    label = "Receiver:",
                    min = 0, max = 20, value = 10,
                    step = 1)
      ),
      mainPanel()
    )
  )

server <- function(input, output, session) {
  observe({
    val <- input$control
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(
      session,
      inputId = "receive",
      value = val,
      min = floor(val/2), max = val+4,
      step = (val+1) %% 2 + 1)
  })
}

shinyApp(ui = ui, server = server)

# See more at:
# https://shiny.rstudio.com/reference/shiny/1.6.0/
