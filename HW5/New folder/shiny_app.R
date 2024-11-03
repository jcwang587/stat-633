library(shiny)
library(markdown)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)


# Data preparation
happiness2023 <- read.csv("HW5_with_region.csv", stringsAsFactors = TRUE)
colnames <- colnames(happiness2023) %>% .[-c(1, 2, 3)] %>% .[-length(.)]

# Define UI 
ui <- navbarPage(
  theme = shinytheme("journal"),
  title = "World Happiness",
  tabPanel("Plot",
           sidebarPanel(
             width = 3,
             uiOutput("variable_1"),
             uiOutput("variable_2"),
             helpText("ctrl/shift for multiple choice"),
             uiOutput("country"),
             hr(),
             span("Data source:", 
                  tags$a("WHR 2023",
                         href = "http://worldhappiness.report"))),
           mainPanel(plotlyOutput("scatterplot"))),
  
  tabPanel("About", 
           "Please check the same directory as this RMarkdown file for the data 
           file and the R script used for this Shiny app."))


# Define server 
server <- function(input, output, session) {
  
  output$variable_1 <- renderUI({
    selectInput("variable_1", "Y-variable:", 
                choices = colnames, selected = "happiness.score")})
  
  output$variable_2 <- renderUI({
    selectInput("variable_2", "X-variable:", 
                choices = colnames, selected = "happiness.score")})
  
  data_2023 <- reactiveValues(data = happiness2023)
  
  filtered_data <- reactive({
    data_2023$data %>% 
      filter(Country %in% c(input$country)) %>% 
      arrange(Country)
  })
  
  countries <- reactive({ 
    df_small <- 
      happiness2023 %>% 
      select(Country) %>% 
      droplevels()
    c("Select All", levels(df_small$Country))
  })
  
  output$country <- renderUI ({
    selectInput("country", "Countries:", 
                multiple = TRUE,
                choices = countries(), 
                selected = "Select All",
                selectize = FALSE,
                size = 10)
  })
  
  observe({
    if ("Select All" %in% input$country) {
      selected_choices <- setdiff(countries(), "Select All")
      updateSelectInput(session, "country", selected = selected_choices)
    }
  })
  
  output$scatterplot <- renderPlotly({
    if (is.null(data_2023$data)) return()
    
    p <- ggplot(filtered_data()) +
      geom_point(aes_string(x = input$variable_2, 
                            y = input$variable_1, 
                            colour = "Region", 
                            label = "Country"), 
                 size = 3) +
      ggtitle(paste0(input$variable_1, " vs. ", input$variable_2))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)