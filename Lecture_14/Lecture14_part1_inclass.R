# Server-side linking with shiny -----

# The combination of **plotly** and **shiny** makes for a powerful toolkit
# for linking views on the web from R. 

## Embedding plotly in shiny -----

# The most common **plotly**+**shiny** pattern uses a **shiny** input to
# control a **plotly** output. 

# Example: 
# Use **shiny**'s `selectizeInput()` function to create a
# dropdown that determines which cities will be shown in a
# plotly line trace:

library(shiny)
library(plotly)
ui <- fluidPage(
  selectizeInput(
    inputId = "cities", 
    label = "Select a city", 
    choices = unique(txhousing$city), 
    selected = "Abilene",
    multiple = TRUE
  ),
  plotlyOutput(outputId = "p")
)

server <- function(input, output) {
  output$p <- renderPlotly({
    plot_ly(txhousing, x = ~date, y = ~median) %>%
      filter(city %in% input$cities) %>%
      group_by(city) %>%
      add_lines()
  })
}

shinyApp(ui, server)

## Leveraging plotly input events -----

# We want to extend our apps so that **outputs will act like inputs to another outputs**.

# - For example, say we'd like to dynamically generate a bar chart (i.e., an
#   output) based on a point clicked on a scatterplot (i.e., an input event tied
#   to an output widget). 

# The `event_data()` function is the most straightforward way to access
# **plotly** input events in **shiny**. Although `event_data()` is a function,
# it references and returns a **shiny** input value, so `event_data()` needs
# to be used inside a reactive context.

# Most of these available events are data-specific traces (e.g., `"plotly_hover"`,
# `"plotly_click"`, `"plotly_selected"`, etc.), but there are also some that are
# layout-specific (e.g., `"plotly_relayout"`). 

# For a complete list, see the `help(event_data)` documentation page.

### Dragging events 1 - "plotly_selected" -----

# There are currently four different modes for mouse click + drag behavior
# (i.e., `dragmode`) in plotly.js: zoom, pan, rectangular selection, and lasso
# selection. 

# This mode may be changed interactively via the modebar that appears above a
# **plotly** graph, but the default mode can also be set from the command-line.

# * Rectangular/lasso mode -----

# Example: 
# The `mtcars` dataset was extracted from the 1974 Motor Trend US magazine,
# and comprises fuel consumption and 10 aspects of automobile design and
# performance for 32 automobiles (1973–74 models).

# `mpg` = Miles/(US) gallon
# `wt` = Weight (1000 lbs)

# In order to get a sense of the type of input value that `event_data()` returns
# we can, e.g.:
ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("selected")
)

server <- function(input, output) {
  output$plot <- 
    renderPlotly({
      plot_ly(mtcars, x = ~mpg, y = ~wt) %>%
        layout(dragmode = "select")
    })
  
  output$selected <- 
    renderPrint({
      input_selected <- event_data("plotly_selected")
      str(input_selected)
    })
}

shinyApp(ui, server)

# Exercise:
# What do you guess `curveNumber` represents? Verify your guess.

# Answer:






# Exercise:
# Build upon the first app, bu instead of printing the raw values, print:

# Total number of points chosen = (some number)
# Mean weight of selected observations = (some number)
# Standard deviation of weight of selected observations = (some number)
# Mean MPG of selected observations = (some number)
# Standard deviation of MPG of selected observations = (some number)

# Answer:



# A little more: if there are no observations selected please have the total 
# number of points selected = 0; and "Not Applicable" for all other entries.
# Hint: `is.null()`

# Answer:



### Dragging events 2 -----

# We've seen that when in rectangular lasso selection modes, information
# about the drag event can be accessed via "plotly_selected". 

# In total, there are 4 ways to access drag data:
# `"plotly_selecting"`/`"plotly_selected"` and
# `"plotly_brushing"`/`"plotly_brushed"`. 

# Exercise:
# Add to the following a `verbatimTextOuptput`/`renderPrint` for each of the other
# three events (consider also printing the data type you get for each event)
# and explain what they record (examine carefully both rectangle and lasso mode, as well
# as their combinations).

library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("selected")
)

server <- function(input, output) {
  output$plot <- 
    renderPlotly({
      plot_ly(mtcars, x = ~mpg, y = ~wt) %>%
        layout(dragmode = "select")
    })
  
  output$selected <- 
    renderPrint({
      cat("plotly_selected\n")
      event_data("plotly_selected")
    })
}

shinyApp(ui, server)


# Answer:



### Other events -----


# Exercise:
# Understand the structure of the inputs created from:
# "plotly_relayout",
# "plotly_hover" / "plotly_unhover"
# "plotly_click" / "plotly_doubleclick"
# "plotly_legendclick" / "plotly_legenddoubleclick"

# Try this for scatter traces as well as histogram layers.

# Answer:





### Scoping events -----

# This section leverages the interface for accessing **plotly** input events
# to inform other data views about those events. When managing multiple views
# that communicate with one another, you'll need to be aware of which views
# are a *source* of interaction and which are a *target* (a view can be both,
# at once!). 

# The `event_data()` function provides a `source` argument to help refine which
# view(s) serve as the source of an event. The `source` argument takes a string
# ID, and when that ID matches the `source` of a `plot_ly()`/`ggplotly()` graph,
# then the `event_data()` is "scoped" to that view.

# Example for usage of "plotly_click" + `source`

# A 2d-heatmap that represents the correlation matrix for `mtcars`.
# Whenever you click a rectangle in the heatmap, a scatterplot updates and
# shows the marginal relationship between the two variables.

# In this case, the heatmap will be the source.

# Cache computation of the correlation matrix
correlation <- t(round(cor(mtcars), 3))

ui <- fluidPage(
  plotlyOutput("heat", width = 400, height = 300),
  plotlyOutput("scatterplot", width = 350, height = 250)
)

server <- function(input, output) {
  
  output$heat <- renderPlotly({
    # plot_ly(source = "heat_plot") %>%
    plot_ly() %>%
      add_heatmap(
        x = names(mtcars), 
        y = names(mtcars), 
        z = correlation,
        colorscale = "RedBlue"
      )
  })
  
  output$scatterplot <- renderPlotly({
    # Try without `source`:
    clickData <- event_data("plotly_click")
    # Correct with source:
    # clickData <- event_data("ploty_click", source = "heat_plot")
    
    if (is.null(clickData)) { return(NULL) }
    
    # Obtain the clicked x/y variables and fit linear model
    vars <- c(clickData$x, clickData$y)
    d <- setNames(mtcars[vars], c("x", "y"))
    yhat <- fitted(lm(y ~ x, data = d))
    
    # scatterplot with fitted line
    plot_ly(d, x = ~x) %>%
      add_markers(y = ~y) %>%
      add_lines(y = ~yhat) %>%
      layout(
        xaxis = list(title = clickData$x), 
        yaxis = list(title = clickData$y), 
        showlegend = FALSE
      )
  })
  
}

shinyApp(ui, server)


# customdata -----

# When using events to inform another view of the data, it's
# often necessary to know what portion of data was queried in the
# event (i.e., the `x`/`y` positions alone may not be enough to
# uniquely identify the information of interest).

# For this reason, it's often a good idea to supply a `key`
# (and/or `customdata`) attribute, so that you can map the event
# data back to the original data.

# The `key` attribute is only supported in **shiny**, but `customdata` is
# officially supported by plotly.js, and thus can also be used to attach
# meta-information to event.

# Example - customdata

# Access x/y coordinates and rownames of selected points using
# `event_data("plotly_selected")` and `customdata`:
library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("selected")
)

server <- function(input, output) {
  
  output$plot <- 
    renderPlotly({
      plot_ly(
        mtcars, 
        x = ~cyl, 
        y = ~gear
      ) %>%
        layout(dragmode = "select")
    })
  
  output$selected <- 
    renderPrint({
      cat("plotly_selected\n")
      event_data("plotly_selected")
    })
}

shinyApp(ui, server)

# Wait, why shouldn't we just use `pointNumber`?


# Counterexample: pointNumber is relative to trace

# An app that changes the color of selected points and separates them
# into a new layer:
ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("text")
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    select_data <- event_data("plotly_selected", source = "select")
    data <- mtcars
    data$col <- "black"
    if (!is.null(select_data)) {
      idx <- select_data$pointNumber + 1
      data[idx, "col"] <- "blue"
    }
    p <- ggplot(data, aes(mpg, wt, col = I(col))) + geom_point()
    ggplotly(p, source = "select") %>%
      layout(dragmode = "select")
  })
  
  output$text <- 
    renderPrint(
      event_data("plotly_selected", source = "select"))
}

shinyApp(ui, server)

# Notice how we use the event data from within the same plot that
# generates the data!

# Exercise: 
# Fix this using customdata

# Answer:





# Exercise:
# Write an app that expands the following to add two boxplots:

# 1. a boxplot for the variable `qsec` (= 1/4 mile time)
# 2. an adjacent boxplot for the same variable, conditioned on
#    the all observations that are selected in the scatterplot.



library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot", height = 300, width = 600),
  plotlyOutput("boxes", height = 300, width = 600)
)

server <- function(input, output) {
  
  output$plot <- 
    renderPlotly({
      plot_ly(mtcars, x = ~mpg, y = ~wt, customdata = nms) %>%
        layout(dragmode = "select")
    })
  
  output$boxes <- 
    renderPlotly({
      
      box_full <- 
        plot_ly("") %>%
        add_boxplot(y = ~qsec) # %>% ""
      
      sl <- event_data("plotly_selected")
      box_conditional <-
        "" %>%
        filter("" %in% "") %>%
        plot_ly() %>%
        add_boxplot(y = ~qsec) # %>% ""
      
      subplot(box_full, box_conditional)
    })
}

shinyApp(ui, server)


# Answer:






# Note: if we wre to subset only by mpg and look at low values
# of mpg, we would get slightly lower, but still near average
# of the dataset. However, if we take low values of mpg without 
# the three high wt values, we get lower qsec values.


# Example:

# Map the numerical `x` value emitted in a click event back to the
# discrete variable that it corresponds to (`mpg$class`).

# Utilize `customdata` to encode the `fill` mapping allowing
# us to display the data records a clicked bar corresponds to. 

ui <- fluidPage(
  plotlyOutput("bars"),
  verbatimTextOutput("click")
)

classes <- sort(unique(mpg$class))

server <- function(input, output, session) {
  
  output$bars <- renderPlotly({
    ggplot(mpg, aes(class, fill = drv, customdata = drv)) +
      geom_bar()
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) return("Click a bar")
    mpg %>%
      filter(drv %in% d$customdata) %>%
      filter(class %in% classes[d$x])
  })
  
}

shinyApp(ui, server)


### Accumulating and managing event data -----

# Currently all the events accessible through `event_data()` are *transient*.
# This means that, given an event like `"plotly_click"`, the value of
# `event_data()` will only reflect the most recent click information. 

# In order to implement complex linked graphics with *persistent* qualities,
# you'll need some way to accumulate and manage event data.

# The general mechanism that **shiny** provides to achieve this kind of task is
# `reactiveVal()` / `reactiveValues()`.

# Example:

# A shiny app that accumulates hover information and paints the hovered points
# in red. Every time a hover event is triggered, the corresponding car name is
# added to the set of selected cars, and every time the plot is double-clicked
# that set is cleared. 

ui <- fluidPage(
  plotlyOutput("p"),
  tableOutput("table")
)

server <- function(input, output, session) {
  
  # keep track of which cars have been hovered on
  cars <- reactiveVal()
  
  # On hover, the key field of the event data contains the car name
  # Add that name to the set of all "selected" cars
  observeEvent(event_data("plotly_hover"), {
    car <- event_data("plotly_hover")$customdata
    cars_old_new <- c(cars(), car)
    cars(unique(cars_old_new))
  })
  
  # clear the set of cars when a double-click occurs
  observeEvent(event_data("plotly_doubleclick"), {
    cars(NULL)
  })
  
  output$p <- renderPlotly({
    
    # if the car is selected, paint it red
    cols <- ifelse(row.names(mtcars) %in% cars(), "red", "black")
    
    mtcars %>%
      plot_ly(
        x = ~wt, y = ~mpg, 
        customdata = row.names(mtcars), 
        marker = list(color = cols)
      ) %>%
      add_markers()
  })
  
  output$table <- renderTable({
    filter(mtcars, row.names(mtcars) %in% cars())
  })
  
}

shinyApp(ui, server)

# Note: here we are not separating the newly colored points to a new 
# layer - we are manipulating the `marker` object directly.

# Exercise:
# Modify the above app so that hovering again over a red point changes its color
# back to black and removes it from the output table.

# In order to perform this task efficiently, think carefully about what we need to change, 
# in which part of the code it lies. A solution can only involve modifying/expanding
# one line of code.

# Answer:

