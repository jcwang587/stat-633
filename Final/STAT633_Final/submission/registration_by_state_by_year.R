library(stringr)
library(shiny)
library(plotly)
Sys.setenv("MAPBOX_TOKEN" = "your_mapbox_token")
Registr <- read.csv("Registration_2016_2022.csv",header=T)
summary(Registr)
for(i in 2:ncol(Registr)){
  Registr[,i] <- as.numeric(str_replace_all(Registr[,i],",",""))
}
coordinates <- read.csv("us-state-capitals.csv") %>% mutate(State=name)
coordinates$incomemedian2016 <- str_replace(coordinates$incomemedian2016, "\\$","")
coordinates$incomemedian2016 <- as.numeric(str_replace(coordinates$incomemedian2016, ",",""))
coordinates$incomemedian2017 <- str_replace(coordinates$incomemedian2017, "\\$","")
coordinates$incomemedian2017 <- as.numeric(str_replace(coordinates$incomemedian2017, ",",""))
coordinates$incomemedian2018 <- str_replace(coordinates$incomemedian2018, "\\$","")
coordinates$incomemedian2018 <- as.numeric(str_replace(coordinates$incomemedian2018, ",",""))
coordinates$incomemedian2019 <- str_replace(coordinates$incomemedian2019, "\\$","")
coordinates$incomemedian2019 <- as.numeric(str_replace(coordinates$incomemedian2019, ",",""))
coordinates$incomemedian2020 <- str_replace(coordinates$incomemedian2020, "\\$","")
coordinates$incomemedian2020 <- as.numeric(str_replace(coordinates$incomemedian2020, ",",""))
coordinates$incomemedian2021 <- str_replace(coordinates$incomemedian2021, "\\$","")
coordinates$incomemedian2021 <- as.numeric(str_replace(coordinates$incomemedian2021, ",",""))
coordinates$incomemedian2022 <- str_replace(coordinates$incomemedian2022, "\\$","")
coordinates$incomemedian2022 <- as.numeric(str_replace(coordinates$incomemedian2022, ",",""))
coordinates <- arrange(coordinates,State)



Registr <- dplyr::left_join(Registr,coordinates[,c(3,4,12)]) %>%
  mutate(NEV=Plug.In.Hybrid.Electric..PHEV.+Hybrid.Electric..HEV.+
           Biodiesel+Ethanol.Flex..E85.+Compressed.Natural.Gas..CNG.+Propane+
           Hydrogen+Methanol+Gasoline+Diesel+Unknown.Fuel,
         EV_percentage=round(Electric..EV./NEV,4))%>%arrange(Year,State)%>%filter(State!="United States")
for(i in 1:7){
  Registr[i+357,'Year']  <- 2015+i
  Registr[i+357,'State']  <- "Average"
  Registr[i+357,'EV_percentage'] <- mean(Registr$EV_percentage[((i-1)*51+1):(i*51)])
}
Registr$Median_Income <- c(coordinates$incomemedian2016,coordinates$incomemedian2017,coordinates$incomemedian2018,coordinates$incomemedian2019,coordinates$incomemedian2020,coordinates$incomemedian2021,coordinates$incomemedian2022,rep(0,7))
Registr <- arrange(Registr,Year,State)

COLs <- colorRamp(c("green", "orange"))



ui <- fluidPage(
  sliderInput(
    inputId = "year_selected", 
    label = "Choose a year", 
    value = 2016, min = 2016, max = 2022)
  ,
  fluidRow(column(6,plotlyOutput("MAP",height="250px")),
  column(6,plotlyOutput("correlation",height="250px",width="80%"))),
  plotlyOutput("trend_plot",heigh="200px"))


input_selected_multiple <- "Average"
server <- function(input, output) {
  YEAR <- reactive({
    input$year_selected
  })
  output$MAP <-
    renderPlotly({
      Registr%>% filter(Year==YEAR(),State!="Average") %>%
        plot_mapbox() %>%
        add_markers(
          x = ~longitude,
          y = ~latitude,
          color = ~Median_Income,colors=COLs, size=~EV_percentage,opacity=0.8,
          text = ~paste0(State,"\nEV_percentage: ",EV_percentage,"\nMedian_Income: ",Median_Income),
          #name=~State,
          hoverinfo = "text+color")%>%
        colorbar(title ="Median_Income")%>%
        layout(
          mapbox = list(
            style = 'open-street-map',
            zoom =1.2,
            center = list(lon = -114, lat = 43)),title="EV_registered_percentage_by_states")
    })
  output$correlation <- 
    renderPlotly({
      Registr%>% filter(Year==YEAR(),State!="Average") %>%
        plot_ly()%>%
        add_markers(
          x=~Median_Income,
          y=~EV_percentage,
          text=~State,
          hoverinfo="x+y+text",
        )%>%layout(title=paste0("cor=",round((Registr%>% filter(Year==YEAR(),State!="Average")%>%select(Median_Income,EV_percentage)%>%
                                         do(corr=cor.test(.$Median_Income,.$EV_percentage)))$corr[[1]]$estimate,4)," ,p-value=",round((Registr%>% filter(Year==YEAR(),State!="Average")%>%select(Median_Income,EV_percentage)%>%
                                                   do(corr=cor.test(.$Median_Income,.$EV_percentage)))$corr[[1]]$p.value,4)))
    })
  output$trend_plot <-
    renderPlotly({
      input_selected <- coordinates[event_data("plotly_click")$pointNumber+1,'State']
      input_selected_multiple <<- c(input_selected_multiple,input_selected)
      Registr%>% filter(State%in%input_selected_multiple) %>%
        plot_ly(x=~Year,y=~EV_percentage,type="scatter",mode="lines",color=~State,name=~State)
      #add_markers(
      #  x = ~Year,
      #  y = ~EV_percentage,
      #  color = ~State,
      #  text = ~State,
      #name=~State,
      #hoverinfo = "x+y+text")
    })
}


shinyApp(ui = ui, server = server)






