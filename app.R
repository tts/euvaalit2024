library(tidyverse)
library(leaflet)
library(shiny)
library(sf)

alue_top <- read_rds("alue_top.RDS")
puolueet <- unique(alue_top$puolue)

shiny::shinyApp(

  ui <- fluidPage(
    
    titlePanel(title = "Suosituimpien puolueiden osuus äänistä äänestysalueittain EU-vaaleissa 2024",
               windowTitle = "EU-vaalit 2024 Helsingissä"),
    
    sidebarPanel(
      selectInput(inputId = "puolueet",
                  label = "Puolue",
                  choices = sort(puolueet),
                  multiple = FALSE,
                  selected = "KOK"),
      tags$div(class="form-group shiny-input-container", 
               HTML("<p><a href='https://github.com/tts/euvaalit2024'>Koodi</a></p>")),
      width = 3),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Kartta", leafletOutput("map", height = 800)),
      ),
      width = 9
    )
  ),
  
  server = function(input, output, session) {
    
    alue_selected <- reactive({
      alue_top %>% 
        filter(puolue == input$puolueet)
    })
    
    output$map <- renderLeaflet({
      
      m <- leaflet() %>%
        addTiles(attribution = 'Oikeusministeriö | avoindata.fi | @ttso') %>%
        setView(lat = 60.192059, lng = 24.945831, zoom = 11) %>%
        addPolygons(data = alue_selected(),
                    color = alue_selected()$col,
                    weight = 3,
                    fillOpacity = 0,
                    opacity = 1,
                    label = paste0(alue_selected()$nimi_fi, ": ", alue_selected()$osuus, "%"))
      
    })
  
  }

)

