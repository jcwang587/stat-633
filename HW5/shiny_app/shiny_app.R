library(shiny)
library(markdown)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)


# Data preparation
hw_data <- read.csv("HW5.data.csv", stringsAsFactors = TRUE)
countries_all <- read.csv("regions.csv", stringsAsFactors = TRUE)
combined_data <- left_join(hw_data, countries_all, by = "Country")
write.csv(combined_data, "HW5_with_regions.csv")
happiness2023 <- read.csv("HW5_with_regions.csv", stringsAsFactors = TRUE)
colnames <- colnames(happiness2023) %>% .[-c(1, 2, 3)] %>% .[-length(.)]

# Define UI 
ui <- navbarPage(
  theme = shinytheme("journal"),
  title = "World Happiness",
  tabPanel("Plot",
           sidebarPanel(
             width = 3,
             helpText("Choose variables you want to see"),
             uiOutput("variable_1"),
             uiOutput("variable_2"),
             helpText("Click while pressing `ctrl / shift` to choose multiple countries"),
             uiOutput("country"),
             hr(),
             span("Data source:", 
                  tags$a("WHR 2023",
                         href = "http://worldhappiness.report"))),
           mainPanel(
             plotlyOutput("scatterplot", height = 500))),
  
  tabPanel("About",
           includeMarkdown("About.md")))
      

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
      # choose all the choices _except_ "Select All"
      selected_choices <- setdiff(countries(), "Select All")
      updateSelectInput(session, "country", selected = selected_choices)
    }
  })

  output$scatterplot <- renderPlotly({
    if (is.null(data_2023$data)) return()
    
    p <- ggplot(filtered_data()) +
      geom_point(aes_string(x = input$variable_2, y = input$variable_1, 
                            colour = "Region", label = "Country"), size = 3) +
      ggtitle(paste0(input$variable_1, " vs. ", input$variable_2)) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("label", "x", "y"), height = 500) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

