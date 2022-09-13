# load libraries
library(shiny)
library(leaflet)

# set timezone to use
Sys.setenv(TZ='UTC')

##########################
# Now look and feel / UI #
##########################
ui <- bootstrapPage(
  tags$style(type = "text/css", "#map {height: calc(100vh - 0px) !important;}"),
  leafletOutput("map")
)

# server logic to draw map
server <- function(input, output, session) {
  # WMS base for ECMWF
  wms_basurl <- "https://apps.ecmwf.int/wms/?token=public&"
  # generate base map
  output$map <- renderLeaflet({
    m <- leaflet() %>% addProviderTiles("Esri.WorldImagery") %>% addScaleBar(position = "bottomleft") %>%
      setView(lat = 46.223551, lng = 6.146139, zoom = 6) %>%
      addWMSTiles(wms_basurl, layers = "genesis_td", options = WMSTileOptions(format = "image/png"))
    m
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)