library(shiny)
library(ggplot2)
library(dplyr)
library(gapminder)

df <- readRDS("gapminder.rds") 

ui <- fluidPage(
  titlePanel("Gapminder"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Interactive plotting of gapminder data using R shiny"),
      
      # For continents:
      checkboxGroupInput("continent",
                         "Choose a continent",
                         choices = levels(df$continent),
                         selected = levels(df$continent)),
      
      # Slider for selecting income percentiles
      sliderInput("incomePercentile",
                  "Income Percentiles of interest",
                  min = 0,
                  max = 100,
                  value = c(0, 100)),
      
      # Slider for selecting a single year, with steps of 5 years
      sliderInput("year",
                  "Year",
                  min = min(df$year),
                  max = max(df$year), 
                  value = min(df$year), 
                  step = 5),
      
      uiOutput("playPauseButton")
      
    ),
    
    mainPanel(
      plotOutput("gapminderPlot")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to control the animation state
  animating <- reactiveVal(FALSE)
  
  # Dynamically render the play/pause button based on the animation state
  output$playPauseButton <- renderUI({
    if(animating()) {
      actionButton("play", label=NULL, icon = icon("pause"), style="border: none; background-color: transparent;", title = "Pause")
    } else {
      actionButton("play", label=NULL, icon = icon("play"), style="border: none; background-color: transparent;", title = "Play")
    }
  })
  
  observeEvent(input$play, {
    animating(!animating())  # Toggle the animation state
  })
  
  observe({
    if (animating()) {
      # Schedule this observer to re-execute with a delay of 10 seconds
      invalidateLater(750, session)
      
      # Isolate to avoid this reactive from re-running when input$year changes
      isolate({
        currentYear <- input$year
        if (currentYear < max(df$year)) {
          newYear <- currentYear + 5
          updateSliderInput(session, "year", value = newYear)
        } else {
          updateSliderInput(session, "year", value = min(df$year))
          animating(FALSE)
        }
      })
    }
  })
  
  # This reactive expression will filter the data according to the inputs
  filteredData <- reactive({
    df %>% 
      filter(continent %in% input$continent,
             year == input$year) %>% 
      arrange(gdpPercap) %>% 
      slice(floor((n()/100) * input$incomePercentile[1]):ceiling((n()/100) * input$incomePercentile[2]))
  })
  
  
  # Render the ggplot
  output$gapminderPlot <- renderPlot({
    # Determine the global range for gdpPercap and lifeExp across all data
    x_limits <- range(df$gdpPercap, na.rm = TRUE)
    y_limits <- range(df$lifeExp, na.rm = TRUE)
    
    ggplot(filteredData(), aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
      geom_point(alpha = 0.7) +
      scale_x_log10(limits = x_limits) + # Fixed x-axis limits
      scale_y_continuous(limits = y_limits) + # Fixed y-axis limits
      theme_gray() +
      labs(x = "gdpPercap",
           y = "lifeExp",
           color = "Continent",
           size = "Population") +
      theme(
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()
        )
  })
}

shinyApp(ui, server)
