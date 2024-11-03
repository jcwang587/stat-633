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

# Packages that are built on the **htmlwidgets** standard (e.g., **plotly**
# and **leaflet**) are, in some sense, also **shiny** output widgets that
# are encouraged to follow this same naming convention 
# (e.g., `renderPlotly()`/`plotlyOutput()` and
# `renderLeaflet()`/`leafletOutput()`).

## Leveraging plotly input events -----

# We want to extend our apps so that **outputs will act like inputs to another outputs**.

# - For example, say we'd like to dynamically generate a bar chart (i.e., an
#   output) based on a point clicked on a scatterplot (i.e., an input event tied
#   to an output widget). 

# In addition to **shiny**'s static graph and image
# rendering functions (e.g., `plotOutput()`/`imageOutput()`), there are a handful
# of other R packages that expose user interaction with "output" widget(s) as
# input value(s). 

# The `event_data()` function is the most straightforward way to access
# **plotly** input events in **shiny**. Although `event_data()` is a function,
# it references and returns a **shiny** input value, so `event_data()` needs
# to be used inside a reactive context.

# Most of these available events are data-specific traces (e.g., `"plotly_hover"`,
# `"plotly_click"`, `"plotly_selected"`, etc.), but there are also some that are
# layout-specific (e.g., `"plotly_relayout"`). 

# For a complete list, see the `help(event_data)` documentation page.

# Numerous figures in the following sections show how to access common
# **plotly** events in **shiny** and do something with the result. When using
# these events to inform another view of the data, it's often necessary to know
# what portion of data was queried in the event (i.e., the `x`/`y` positions
# alone may not be enough to uniquely identify the information of interest).

# For this reason, it's often a good idea to supply a `key` (and/or `customdata`)
# attribute, so that you can map the event data back to the original data.
# The `key` attribute is only supported in **shiny**, but `customdata` is
# officially supported by plotly.js, and thus can also be used to attach
# meta-information to event.

### Dragging events -----

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

# Access x/y coordinates of selected points using `event_data("plotly_selected")`:
library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("selected")
)

server <- function(input, output) {
    output$plot <- 
      renderPlotly({
        plot_ly(mtcars, x = ~mpg, y = ~wt) #%>%
          # layout(dragmode = "select")
        })

  output$selected <- 
    renderPrint({
      # cat("plotly_selected\n")
      event_data("plotly_selected")
      })
}

shinyApp(ui, server)

# Exercise:
# What do you guess `curveNumber` represents? Verify your guess.

# Answer:
# It represents the number of a trace a point is in:
ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("selected")
)

server <- function(input, output) {
  output$plot <- 
    renderPlotly({
      plot_ly(mtcars, x = ~mpg) %>%
        add_markers(y = ~wt) %>%
        add_markers(y = ~drat)
      # layout(dragmode = "select")
    })
  
  output$selected <- 
    renderPrint({
      # cat("plotly_selected\n")
      event_data("plotly_selected")
    })
}

shinyApp(ui, server)

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
      # cat("plotly_selected\n")
      input_selected <- event_data("plotly_selected")
      str(input_selected)
    })
}

shinyApp(ui, server)

# Exercise:
# Instead of printing the raw values, print:

# Total number of points chosen = (some number)
# Mean weight of selected observations = (some number)
# Standard deviation of weight of selected observations = (some number)
# Mean MPG of selected observations = (some number)
# Standard deviation of MPG of selected observations = (some number)

# An answer:
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
      input_selected <- event_data("plotly_selected")
      cat("Total number of points chosen = ", nrow(input_selected), "\n")
      cat("Mean weight of selected observations = ", round(mean(input_selected$y), 2), "\n")
      cat("Standard deviation of weight of selected observations = ", round(sd(input_selected$y), 2), "\n")
      cat("Mean MPG of selected observations = ", round(mean(input_selected$x), 2), "\n")
      cat("Standard deviation of MPG of selected observations = ", round(sd(input_selected$x), 2), "\n")
    })
}

shinyApp(ui, server)

# A little more: if there are no observations selected please have the total 
# number of points selected = 0; and "Not Applicable" for all other entries.
# Hint: `is.null()`

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
      input_selected <- event_data("plotly_selected")
      if (!(is.null(input_selected) | length(input_selected) == 0 ) )  {
        cat("Total number of points chosen = ", nrow(input_selected), "\n")
        cat("Mean weight of selected observations = ", round(mean(input_selected$y), 2), "\n")
        cat("Standard deviation of weight of selected observations = ", round(sd(input_selected$y), 2), "\n")
        cat("Mean MPG of selected observations = ", round(mean(input_selected$x), 2), "\n")
        cat("Standard deviation of MPG of selected observations = ", round(sd(input_selected$x), 2))
      } else {
        cat("Total number of points chosen = 0\n")
        cat("Mean weight of selected observations = Not Applicable\n")
        cat("Standard deviation of weight of selected observations = Not Applicable\n")
        cat("Mean MPG of selected observations = Not Applicable\n")
        cat("Standard deviation of MPG of selected observations = Not Applicable")
      }
    })
}

shinyApp(ui, server)


# So we've seen that when in rectangular lasso selection modes, information
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
      # cat("plotly_selected\n")
      event_data("plotly_selected")
    })
}

shinyApp(ui, server)


# Answer:
library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("selected"),
  verbatimTextOutput("selecting"),
  verbatimTextOutput("brushed"),
  verbatimTextOutput("brushing")
)

server <- function(input, output) {
  output$plot <- 
    renderPlotly({
      plot_ly(mtcars, x = ~mpg, y = ~wt) %>%
        layout(dragmode = "select") %>%
        event_register("plotly_selecting")
    })
  
  output$selected <- 
    renderPrint({
      cat("plotly_selected\n")
      input_selected <- event_data("plotly_selected")
      str(input_selected)
    })
  
  output$selecting <- 
    renderPrint({
      cat("plotly_selecting\n")
      input_selecting <- event_data("plotly_selecting")
      str(input_selecting)
    })
  
  output$brushed <- 
    renderPrint({
      cat("plotly_brushed\n")
      input_brushed <- event_data("plotly_brushed")
      print(input_brushed)
      str(input_brushed)
    })
  
  output$brushing <- 
    renderPrint({
      cat("plotly_brushing\n")
      input_brushing <- event_data("plotly_brushing")
      print(input_brushing)
      str(input_brushing)
    })
}

shinyApp(ui, server)

# Both the `"plotly_selecting"` and `"plotly_selected"` events emit information
# about trace(s) appearing within the interior of the brush; the only difference
# is that `"plotly_selecting"` fires repeatedly *during* drag events, whereas
# `"plotly_selected"` fires *after* drag events (i.e., after the mouse has been
# released). 

# The semantics behind `"plotly_brushing"` and `"plotly_brushed"` are similar, but
# these emit the x/y limits of the selection brush. 
# Note that brushing events only return the coordinates of the last brushing event
# whereas the selecting events accumulate all points (more about how to do that
# later).

# * Other events -----

# Exercise:
# Understand the structure of the inputs created from:
# "plotly_relayout",
# "plotly_hover" / "plotly_unhover"
# "plotly_click" / "plotly_doubleclick"
# "plotly_legendclick" / "plotly_legenddoubleclick"

# Try this for scatter traces as well as histogram layers.

# An answer:
ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("relayout")
)

server <- function(input, output) {
  output$plot <- 
    renderPlotly({
      plot_ly(mtcars, x = ~mpg, y = ~wt) %>%
        config(scrollZoom = TRUE)
    })
  
  output$relayout <- 
    renderPrint({
      str(event_data("plotly_relayout"))
    })
}

shinyApp(ui, server)


# An answer 2:
ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("hover"),
  verbatimTextOutput("unhover")
)

server <- function(input, output) {
  output$plot <- 
    renderPlotly({
      plot_ly(mtcars, x = ~mpg, y = ~wt) %>%
        event_register("plotly_unhover")
    })
  
  output$hover <- 
    renderPrint({
      event_data("plotly_hover")
    })
  
  output$unhover <- 
    renderPrint({
      event_data("plotly_unhover")
    })
}

shinyApp(ui, server)

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
  
  # nms <- row.names(mtcars)
  
  output$plot <- 
    renderPlotly({
      plot_ly(
        mtcars, 
        # customdata = nms,
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
ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("text")
)

server <- function(input, output, session) {
  
  data <- mtcars
  data$customdata <- 1:nrow(data)
  data$col <- "black"
  
  output$plot <- renderPlotly({
    select_data <- event_data("plotly_selected", source = "select")
    if (!is.null(select_data)) {
      idx <- data$customdata %in% select_data$customdata
      data[idx, "col"] <- "blue"
    }
    p <- ggplot(data, aes(mpg, wt, col = I(col),
                          customdata = customdata)) + geom_point()
    ggplotly(p, source = "select") %>%
      layout(dragmode = "select")
  })
  
  output$text <- 
    renderPrint(
      event_data("plotly_selected", source = "select"))
}

shinyApp(ui, server)


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

library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot", height = 300, width = 600),
  plotlyOutput("boxes", height = 300, width = 600)
)

server <- function(input, output) {
  
  data <- mtcars
  nms <- 1:nrow(mtcars)
  
  output$plot <- 
    renderPlotly({
      plot_ly(mtcars, x = ~mpg, y = ~wt,
              customdata = nms, text = rownames(data),
              hoverinfo = "x+y+text") %>%
        layout(dragmode = "select")
    })
  
  output$boxes <- 
    renderPlotly({
      box_full <- 
        plot_ly(data = mtcars) %>%
        add_boxplot(y = ~qsec) %>%
        layout(yaxis = list(range = range(data$qsec)))
      sl <- event_data("plotly_selected")
      box_conditional <-
        mtcars %>%
          filter(nms %in% sl$customdata) %>%
          plot_ly() %>%
          add_boxplot(y = ~qsec) %>%
        layout(yaxis = list(range = range(data$qsec)))
      subplot(box_full, box_conditional)
    })
}

shinyApp(ui, server)

# Note: if we wre to subset only by mpg and look at low values
# of mpg, we would get slightly lower, but still near average
# of the dataset. However, if we take low values of mpg without 
# the three high wt values, we get lower qsec values.


# Example:

# For events that are trace-specific (e.g., `"plotly_click"`, `"plotly_hover"`,
# `"plotly_selecting"`, etc.), the positional data (e.g., `x`/`y`/`z`) is always
# numeric, so if you have a plot with discrete axes, you might want to know how
# to map that numeric value back to the relevant input data category. In some
# cases, you can avoid the problem by assigning the discrete variable of interest
# to the `key`/`customdata` attribute, but you might also want to reserve that
# attribute to encode other information, like a `fill` aesthetic. 

# Map the numerical `x` value emitted in a click event back to the
# discrete variable that it corresponds to (`mpg$class`).

# Utilize `customdata` to encode the `fill` mapping allowing
# us to display the data records a clicked bar corresponds to. 

# In both `ggplotly()` and `plot_ly()`, categories associated with a character
# vector are always alphabetized, so if you `sort()` the `unique()` character
# values, then the vector indices will match the `x` event data values. On the
# other hand, if `x` were a factor variable, the `x` event data would match the
# ordering of the `levels()` attribute.

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






### Partial plotly updates -----

# When `renderPlotly()` renders a new **plotly** graph, it generates a new
# **plotly** graph from scratch. Not only does the R code need to re-execute
# to generate a new R object, but it also has to re-serialize that object as
# JSON, and your browser has to re-render the graph from the new JSON object.

# In cases where your **plotly** graph does not need to serialize a lot of data
# and/or render lots of graphical elements you can likely perform a full redraw
# without noticeable glitches, especially if use Canvas-based rendering rather
# than SVG (i.e., `toWebGL()`). 

# On initial page load, **plotly** graphs must be drawn from stratch, but when
# responding to certain user events, often a partial update to an existing plot
# is sufficient and more responsive. 

# Example:

# Partial update for path size:

library(shiny)
library(plotly)

ui <- fluidPage(
  sliderInput("path", "Path size", min = 0, max = 30, value = 2),
  plotlyOutput("p")
)

server <- function(input, output, session) {
  
  output$p <- renderPlotly({
    plot_ly(
      economics, x = ~pce, y = ~psavert, z = ~unemploy, 
      color = ~as.numeric(date), mode = "lines"
    )
  })
  
  observeEvent(
    input$path,
    {
      plotlyProxy("p", session) %>%
        plotlyProxyInvoke(
          "restyle", list(line.width = input$path)
        )
      }
    )
}

shinyApp(ui, server)

# What is going on here?

# 1. We use `observeEvent` to run some code only when `input$path` is changed.
# 2. When 1 happens, we call `plotlyProxy("p", session)`.
#    - "p" here corresponds to an output element ID (`plotlyOutput("p")`)
#    - `session` is the shiny session. We have to add the argument `session` to the
#      server's definition: `server <- function(input, output, session)`,
#      everything else with the session happens behind the scenes.
#    - plotlyProxy creates a proxy object that references the relevant output ID.
# 3. We use the proxy object as input for `plotlyProxyInvoke()` (hence the %>%).
#    - `plotlyProxyInvoke()` invokes any sequence of plotly.js function(s) on the proxy 
#      object.
#    - The second argument of `plotlyProxyInvoke()` is a `method` argument.
#      In this example, "restyle". As its name suggests, we can use it to modify
#      properties that relate to the style of objects - line widths, marker sizes etc.
#    - The following arguments are objects that are sent to plotly.js.
#      In this example, we send `list(line.width = input$path)` and change the line width!

# * Invoking a method with the correct arguments can be tricky and requires knowledge
#   of plotly.js because plotlyProxyInvoke() will send these arguments directly to the
#   plotly.js method and therefore doesn’t support the same ‘high-level’ semantics that
#   `plot_ly()` does.


# Exercise:
# Add to the previous app an input slider for marker size, and 
# use a partial updating method to change the marker size.


# Start from the following, consult the refernece guide, modify the "---" strings.
library(shiny)
library(plotly)

ui <- fluidPage(
  sliderInput("marker", "Marker size", min = 0, max = 20, value = 8),
  sliderInput("path", "Path size", min = 0, max = 30, value = 2),
  plotlyOutput("p")
)

server <- function(input, output, session) {
  
  output$p <- renderPlotly({
    plot_ly(
      economics, x = ~pce, y = ~psavert, z = ~unemploy, 
      color = ~as.numeric(date), mode = "markers+lines"
    )
  })
  
  observeEvent("---", {
    plotlyProxy("---", session) %>%
      plotlyProxyInvoke(
        "restyle", 
        "---"
      )
  })
  
  observeEvent(input$path, {
    plotlyProxy("p", session) %>%
      plotlyProxyInvoke(
        "restyle", list(line.width = input$path)
      )
  })
  
}

shinyApp(ui, server)


# Answer:
library(shiny)
library(plotly)

ui <- fluidPage(
  sliderInput("marker", "Marker size", min = 0, max = 20, value = 8),
  sliderInput("path", "Path size", min = 0, max = 30, value = 2),
  plotlyOutput("p")
)

server <- function(input, output, session) {
  
  output$p <- renderPlotly({
    plot_ly(
      economics, x = ~pce, y = ~psavert, z = ~unemploy, 
      color = ~as.numeric(date), mode = "markers+lines"
    )
  })
  
  observeEvent(input$marker, {
    plotlyProxy("p", session) %>%
      plotlyProxyInvoke(
        "restyle", 
        list(marker.size = input$marker)
      )
  })
  
  observeEvent(input$path, {
    plotlyProxy("p", session) %>%
      plotlyProxyInvoke(
        "restyle", list(line.width = input$path)
      )
  })
  
}

shinyApp(ui, server)

# Exercise - update scattermapbox -----

# Modify the mapp of Canadian cities so that zooming in resizes the markers, 
# but does so with a partial update.

# Start from:
library(shiny)
library(plotly)
library(tidyverse)

can_cities <- maps::canada.cities

ui <- fluidPage(
  plotlyOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderPlotly({
    p <-
      plot_mapbox(can_cities) %>%
      add_markers(
        x = ~long, 
        y = ~lat,
        size = ~pop,       # high-level plot_ly semantics.
        sizes = c(1, 10)   # high-level plot_ly semantics.
      ) %>%
      layout(
        mapbox =
          list(
            center = list(lat = 70.6, lon = -94.5),
            zoom = 1
          )
      )
  })
  
  observeEvent("---", {
    z <- "---"
    if (is.null(z)) { z <- 1 }  # in the event the window is resized

    plotlyProxy("---", session) %>%
      plotlyProxyInvoke(
        "restyle",
        "---"
      )
  })
}

shinyApp(ui, server)


# Answer:

library(shiny)
library(plotly)
library(tidyverse)

can_cities <- maps::canada.cities

ui <- fluidPage(
  plotlyOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderPlotly({
    p <-
      plot_mapbox(can_cities) %>%
      add_markers(
        x = ~long, 
        y = ~lat,
        # size = ~pop,
        # sizes = c(1, 10)
        marker =
          list(
            size = 2 + 10 * (can_cities$pop / max(can_cities$pop)) ^ 0.3
          )
      ) %>%
      layout(
        mapbox =
          list(
            center = list(lat = 70.6, lon = -94.5),
            zoom = 1
          )
      )
  })
  
  observeEvent(event_data("plotly_relayout"), {
    z <- event_data("plotly_relayout")$mapbox.zoom
    if (is.null(z)) { z <- 1 }  # in the event the window is resized
    plotlyProxy("map", session) %>%
      plotlyProxyInvoke(
        "restyle",
        marker = 
          list(
            size = 2 + z * 10 * (can_cities$pop / max(can_cities$pop)) ^ 0.3
          )
      )
  })
}

shinyApp(ui, server)


# One un-intuitive thing about `Plotly.restyle()` is that it fully replaces
# object (i.e., attributes that contain attributes) definitions like `marker`
# by default. To modify just a particular attribute of an object, like the size
# of a marker, you must replace that attribute directly (hence `marker.size`).
# As mentioned in the
# [official documentation](https://plot.ly/javascript/plotlyjs-function-reference/#plotlyrestyle),
# by default, modifications are applied to all traces, but specific traces can be
# targeted through their trace index (which starts at 0, because of JavaScript)!


# To see all available methods that can be invoked, enter:
plotly:::plotlyjs_methods()

# Example - update the layout -----

# All **plotly** figures have two main components: traces (i.e., mapping from data
# to visuals) and layout. The plotly.js function `Plotly.relayout()` modifies the
# layout component, so it can control a wide variety of things such as titles,
# axis definitions, annotations, shapes, and many other things.

# Examine:
p <- ggplot(txhousing) +
  geom_line(aes(date, median, group = city))

ggplotly(p, dynamicTicks = TRUE) %>%
    rangeslider() 
  
# The y-range is fixed. We can use partial updates of the layout (utilizing 
# `Plotly.relayout()`) to automatically adjust the y-range to changes in the x-range.

library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot")
)

server <- function(input, output, session) {
  p <- ggplot(txhousing) +
    geom_line(aes(date, median, group = city))
  
  output$plot <- renderPlotly({
    ggplotly(p, dynamicTicks = TRUE) %>% 
      rangeslider() 
  })
  
  observeEvent(event_data("plotly_relayout"), {
    d <- event_data("plotly_relayout")
    # The data structure emitted is different depending on 
    # whether the relayout is triggered from the rangeslider or the plot:
    xmin <- if (length(d[["xaxis.range[0]"]])) d[["xaxis.range[0]"]] else d[["xaxis.range"]][1]
    xmax <- if (length(d[["xaxis.range[1]"]])) d[["xaxis.range[1]"]] else d[["xaxis.range"]][2]
    if (is.null(xmin) || is.null(xmax)) return(NULL)
    
    # compute the y-range based on the new x-range
    idx <- with(txhousing, xmin <= date & date <= xmax)
    yrng <- extendrange(txhousing$median[idx])
    
    plotlyProxy("plot", session) %>%
      plotlyProxyInvoke("relayout", list(yaxis = list(range = yrng)))
  })
  
  yRange <- range(txhousing$median, na.rm = TRUE)
  observeEvent(event_data("plotly_doubleclick"), {
    
    plotlyProxy("plot", session) %>%
      plotlyProxyInvoke("relayout", list(yaxis = list(range = yRange)))
    
  })
}

shinyApp(ui, server)


# Example - add and delete traces -----

# Both of the following apps display a scatterplot with 100,000 points and allow a
# user to overlay a fitted line through a checkbox. 

# In the first app the **plotly** graph is regenerated from scratch every time
# the value of `input$smooth` changes.

# In the second app only the fitted line is added/removed from the **plotly**. 

# Full redraw:
library(shiny)
library(plotly)

# Generate 100,000 observations from 2 correlated random variables:
s <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
d <- MASS::mvrnorm(1e6, mu = c(0, 0), Sigma = s)
d <- setNames(as.data.frame(d), c("x", "y"))

# fit a simple linear model
m <- lm(y ~ x, data = d)

# generate y predictions over a grid of 10 x values
dpred <- 
  data.frame(
    x = seq(min(d$x), max(d$x), length.out = 10)
  )
dpred$yhat <- predict(m, newdata = dpred)

ui <- fluidPage(
  plotlyOutput("scatterplot"),
  checkboxInput(
    "smooth", 
    label = "Overlay fitted line?", 
    value = FALSE
  )
)

server <- function(input, output, session) {
  output$scatterplot <- renderPlotly({
    p <- plot_ly(d, x   = ~x, y = ~y) %>%
      add_markers(color = I("black"), alpha = 0.05) %>%
      toWebGL() %>%
      layout(showlegend = FALSE)
    
    if (!input$smooth) {
      p
    } else {
      add_lines(p, data = dpred, x = ~x, y = ~yhat, color = I("red"))
    }
  })
}

shinyApp(ui, server)

# Partial update:

# The only difference resides in the `server` definition. 

# The `renderPlotly()` statement no longer has a dependency on input values, so that
# code is only executed once (on page load) to generate the initial view of the
# scatterplot. 

# The logic behind adding and removing the fitted line is handled through an `observe()`
# block; this reactive expression watches the `input$smooth` input value and modifies the
# `output$scatterplot` widget whenever it changes. To trigger a modification of a **plotly**
# output widget, you must create a proxy object with `plotlyProxy()` that references the
# relevant output ID. Once a proxy object is created, you can invoke any sequence of
# [plotly.js function(s)](https://plot.ly/javascript/plotlyjs-function-reference/#plotlymaketemplate) 
# on it with `plotlyProxyInvoke()`. Invoking a method with the correct arguments can be
# tricky and requires knowledge of plotly.js because `plotlyProxyInvoke()` will send these
# arguments directly to the plotly.js method and therefore doesn't support the same
# 'high-level' semantics that `plot_ly()` does.

server <- function(input, output, session) {
  output$scatterplot <- renderPlotly({
    p <-
      plot_ly(d, x = ~x, y = ~y) %>%
      add_markers(color = I("black"), alpha = 0.05) %>%
      toWebGL() %>%
      layout(showlegend = FALSE)
  })
  
  observe({
    if (input$smooth) {
      # this is essentially the plotly.js way of doing
      # `p %>% add_lines(x = ~x, y = ~yhat) %>% toWebGL()`
      # without having to redraw the entire plot
      plotlyProxy("scatterplot", session) %>%
        plotlyProxyInvoke(
          "addTraces", 
          list(
            x = dpred$x,
            y = dpred$yhat,
            type = "scattergl",
            mode = "lines",
            line = list(color = "red")
          )
        )
    } else {
      # JavaScript index starts at 0, so the '1' here really means
      # "delete the second traces (i.e., the fitted line)"
      plotlyProxy("scatterplot", session) %>%
        plotlyProxyInvoke("deleteTraces", 1)
    }
  })
}

shinyApp(ui, server)

### Cross-filter - Diamonds app -----

# Recall our diamonds app from the introduction. We can now understand it!

library(shiny)
library(plotly)
library(dplyr)

diamonds$log_carat <- log(diamonds$carat)

ui <- fluidPage(
  selectInput("varname", "Condition on", c("depth", "log_carat", "table")),
  plotlyOutput("bars", height = 200),
  plotlyOutput("box", height = 200)
)

server <- function(input, output, session) {
  output$bars <- renderPlotly({
    foo <- as.formula(paste0("~", input$varname))
    plot_ly(diamonds, x = foo, source = "bars", nbinsx = 50) %>%
      layout(dragmode = "select", selectdirection = "h") %>% 
      layout(xaxis = list(title = input$varname))
  })
  output$box <- renderPlotly({
    plot_ly(diamonds) %>%
      add_boxplot(x = ~clarity, y = ~log(price)) %>% 
      layout(xaxis = list(title = "Clarity"),
             yaxis = list(title = paste0("log(Price) | ", input$varname),
                          range = c(6, 10)))
  })
  
  observe({
    brush <- event_data("plotly_brushing", source = "bars")
    p <- plotlyProxy("box", session)
    
    if (is.null(brush)) {
      plotlyProxyInvoke(p, "restyle", y = list(log(diamonds$price)))
      return()
    }
    
    c_filter <- 
      diamonds[diamonds[ , input$varname] > brush$x[1] & diamonds[ , input$varname] < brush$x[2], ]
    if (nrow(c_filter) < 10) return()
    plotlyProxyInvoke(p, "restyle", y = list(log(c_filter$price)))
  })
}

shinyApp(ui, server)