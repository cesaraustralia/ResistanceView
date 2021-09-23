library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(DBI)
library(RPostgres)
library(dbplyr)
library(leaflet)
library(terra)
library(sf)
library(DT)

# data preparation ---------------------------------------------------------


# ui ----------------------------------------------------------------------
ui <- shinyUI(
  navbarPage("ResistanceView",
             selected = "Resistance Map", theme = shinytheme("slate"),
             
    tabPanel("Resistance Map",
             
             
             leafletOutput("resmap", height = 800)
               
    )          
  )
)


# server ------------------------------------------------------------------
server <- function(input, output, session){
  
  
  # resistance map
  output$resmap <- renderLeaflet({
    # browser()
    isolate({
      leaflet(options = leafletOptions(zoomControlPosition = "topright")) %>%
        setView(lng = 135.51, lat = -25.98, zoom = 4) %>%
        addTiles() %>%
        addProviderTiles(providers$Stamen.Toner) %>% 
        identity()
    })
  })
  
}


# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
