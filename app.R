#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/


library(shiny)
library(shinydashboard)
library(callr)
library(tidyverse)
library(leaflet)
library(tidyverse)
library(leaflet.extras)
library(sf)
library(shinyWidgets)
library(RColorBrewer)
library(rsconnect)
remove(list=ls())

##Read Data
enrollment_race<-read.csv("enroll_race_info.csv")

#Import Lat Long
areas <- st_read("chicago_shapefile.shp") %>%
  mutate(community = str_to_title(community))

#Make Dataframe backup
enrollment_race_copy <- enrollment_race

#make scatter plot
enrollment_race_white <- enrollment_race_copy %>% filter(race == "White" & SEHS == "SEHS")
enrollment_race_white$year <- as.numeric(enrollment_race_white$year)


#filter for 1 year
enrollment_race <- enrollment_race %>% filter(year== 2019)


# Define UI for application 
ui <- fluidPage(
  leafletOutput("map", height = "750px"),
  absolutePanel(id = "controls", top = 10, left = 10, width = "auto",
                class = "panel panel-default",
                draggable = TRUE, height = 'auto', 
                collapsible = TRUE,
                h4("2019 CPS Enrollment Demographics"),
                
                selectInput(inputId = "race", 
                            label = ("Race Breakdown"),
                            choices = unique(enrollment_race$race)
                ),
                
                radioButtons(inputId = "SEHS",
                              "CPS",
                              c("Other CPS" = "Other CPS", 
                                "SEHS" = "SEHS"),
                              selected = "Other CPS"
                             ),
                
                h6("SEHS % White Student Enrollment Overtime"), 
                plotOutput("pct_white_plot",
                           width = "100%")),
  
)


#41.881832, lng = -87.623177
server <- function(input, output, session) {
  filtered_data<- reactive({
    dplyr::filter(enrollment_race, enrollment_race$race==input$race,
                  enrollment_race$SEHS==input$SEHS)
    
  })
  binpal <- colorBin("YlOrRd", enrollment_race$percent, bins = 9)
  output$map <- renderLeaflet({
    map= leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lat=41.881832, lng = -87.8, zoom = 11) %>%
      addPolygons(data = areas, 
                  color = "white",
                  weight = 1,
                  smoothFactor = 1,
                  opacity = 1.0,
                  fillOpacity = 0.2,
                  fillColor = "purple")

      map= map %>% 
        addCircleMarkers(data = filtered_data(),
                         lng = ~Long, 
                         lat = ~Lat,
                         color = ~binpal(percent),
                         label = ~SCHOOL,
                           #lapply(labs, htmltools::HTML),
                         radius = 2,
                         opacity = 1
        )
map      %>%
        addLegend(pal = binpal,
                  values = enrollment_race$percent,
                  title = "Enrollment (% of School)") 

  })
  output$pct_white_plot <- renderPlot({
    enrollment_race_white %>% ggplot(aes(x=year, y=percent, color = SCHOOL)) + geom_point()+ 
      geom_line() + 
      scale_color_brewer(palette="Set1") + labs(
        x = 'Year',
        y = '% White') + theme(legend.title = element_blank()) +
      scale_x_continuous(breaks=c(2012,2015,2017,2020))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
