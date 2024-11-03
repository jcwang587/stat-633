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
  # sliderInput("path", "Path size", min = 0, max = 30, value = 2),
  plotlyOutput("p")
)

server <- function(input, output, session) {
  
  output$p <- renderPlotly({
    plot_ly(
      economics, x = ~pce, y = ~psavert, z = ~unemploy, 
      color = ~as.numeric(date), mode = "lines"
    )
  })
  
  # observeEvent(
  #   input$path, 
  #   {
  #     plotlyProxy("p", session) %>%
  #       plotlyProxyInvoke(
  #         "restyle", list(line.width = input$path)
  #       )
  #     }
  #   )
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







# Note:
# `Plotly.restyle()` fully replaces object (i.e., attributes that contain attributes)
# definitions like `marker` by default. 

# To modify just a particular attribute of an object, like the size
# of a marker, you must replace that attribute directly (hence `marker.size`).

# As mentioned in the
# [official documentation](https://plot.ly/javascript/plotlyjs-function-reference/#plotlyrestyle),
# by default, modifications are applied to all traces, but specific traces can be
# targeted through their trace index (which starts at 0, because of JavaScript).


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
  
  # observeEvent(event_data("plotly_relayout"), {
  #   d <- event_data("plotly_relayout")
  #   print(d)
  #   
  #   # some code 
  #       
  #   plotlyProxy("plot", session) %>%
  #     plotlyProxyInvoke("relayout", list("something here"))
  # })

  # another something here
  
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

# Recall our diamonds app from the introduction. 

# <https://sta697v.shinyapps.io/diamonds_demo_app/>

# The goal is to condition the box plot:
diamonds$log_carat <- log(diamonds$carat)

varname <- "depth"
plot_ly(diamonds) %>%
  add_boxplot(x = ~clarity, y = ~log(price)) %>% 
  layout(xaxis = list(title = "Clarity"),
         yaxis = list(title = paste0("log(Price) | ", varname),
                      range = c(6, 10)))

# on the selection from the histogram:
foo <- as.formula(paste0("~", varname))
plot_ly(diamonds, x = foo, source = "bars", nbinsx = 50) %>%
  layout(dragmode = "select", selectdirection = "h") %>% 
  layout(xaxis = list(title = varname))


# Let's do it!

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
  
  # observeEvent( "",
    # {
    # Steps:
    # Which event do we want to observe? Study it.
    # Which data do we need from this event?
    # Based on the data we obtain, what transformation do we need to apply?
    # Which proxy object do we need to create?
    # Which method do we need to invoke?
    # What are the relevant arguments for this method?
  # })
}

shinyApp(ui, server)
