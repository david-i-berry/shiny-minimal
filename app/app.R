# load libraries
library(shiny)
library(leaflet)
library(jsonlite)
library(sf)
library(geojsonsf)
library(httr)
library(rnaturalearth)

# set timezone to use
Sys.setenv(TZ='UTC')

# swriXwpl1TuJp4zYxxx9Jw4a

oscarapi <- "https://oscar.wmo.int/surface/#/search/station/stationReportDetails/"

endpoints <- list(
  "Select endpoint" = "",
  "Argentina"= "http://3.125.117.201/oapi/",
  "Australia" = "http://3.124.208.185//oapi/",
  "Canada" = "http://3.68.233.152:8080/oapi/",
  "Italy" = "http://3.122.103.55/oapi/",
  "Fiji"="http://3.71.32.201/oapi/",
  "Demo" = "http://demo.wis2box.wis.wmo.int/oapi/"
)

parameters <- c(
  "wigos_station_identifier",
  "resultTime",
  "name",
  "value",
  "units",
  "description",
  "metadata"
)

# error handling function

fetch_data <- function(url_, headers=""){
  result <- tryCatch({
    response <- GET(
      url_,
      accept_json(),
      add_headers(headers)
    )
    if( response$status_code == 200 ){
      payload <- geojsonsf::geojson_sf(content(response, as="text"))
      status <- 200
    }else{
      message(paste0("Error reading from ",url_))
      message(response$status_code)
      payload <- NULL
      status <- response$status_code
    }
    return(list(status=status, payload = payload))
  },
  error = function(cond){
    message(paste0("Error reading from ",url_))
    message(cond)
    return(list(status=NULL, payload = NULL))
  })
}




# add parameter filter

# add metadata filter

# add wsi filter

##########################
# Now look and feel / UI #
##########################
ui <- navbarPage("WIS2 Demo", id="nav",
                 tabPanel("Map view",
                          tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
                          tags$style(HTML(".leaflet-container { background: #e6f2ff; }")),
                          tags$style(type = "text/css", "#controls, #plots_panel{
                      background-color #ddd;
                      opacity: 0.75;
                      padding: 20px 20px 20px 20px;
                  }
                  #controls:hover, #plots_panel:hover{
                    opacity: 1;
                  }
                  #outer {
                      padding: 0px 0px 0px 0px;
                  }
                  "),
                          leafletOutput("map", width="100%"),
                          absolutePanel(
                            id="controls", class = "panel panel-default", fixed = FALSE,
                            draggable = TRUE, top = 100, left = 20, right = "auto", bottom = "auto",
                            width = 330, height = "auto",
                            h3("Select data"),
                            selectInput("endpoint","WIS2box",choices=endpoints, selected=NULL),
                            selectInput("topic","Topic", ""),
                            sliderInput("timeperiod","Time period",
                                        min=as.POSIXct("2022-09-01", format="%Y-%m-%d"),
                                        max=Sys.time(),
                                        value = c(Sys.time()-3600*3, Sys.time()),
                                        step = 3600, width="100%",
                                        ticks=FALSE, timezone="+0000"),
                            selectInput("wsi","WIGOS Station Identifier", choices=NULL),
                            selectInput("ecv","Parameter", choices=NULL),
                            checkboxInput("showbbox","Show bounding boxes",FALSE)
                          )
                 ),
                 tabPanel("Data",
                          dataTableOutput("datatable")
                 )
)

# server logic to draw map
server <- function(input, output, session) {

  endpoint <- reactive({
    input$endpoint
  })

  topic <- reactive({
    input$topic
  })

  start_time <- reactive({
    input$timeperiod[1]
  })


  end_time <- reactive({
    input$timeperiod[2]
  })

  obs <- reactive({
    if( endpoint() != "" & topic() !=""){
      message("Fetching observations...")
      date_range <- c(start_time(), end_time())
      date_range <- format.POSIXct(date_range,"%Y-%m-%dT%H:%M:00Z")
      date_range <- paste(date_range, collapse = "/")
      url_ <- paste0(
        isolate(input$endpoint),
        "collections/",topic(),"/items?limit=100000&datetime=",date_range#,"&name=pressure_reduced_to_mean_sea_level"
      )
      message(url_)
      thedata <- fetch_data(url_)
      if(thedata$status==200){
        st_zm(thedata$payload, drop=TRUE)
      }else{
        message(paste0("Error loading data, http ", thedata$status))
        NULL
      }
    }else{
      message("No topic and/or endpoint")
      NULL
    }
  })


  observe({
    if(!is.null(obs())){
      updateSelectInput(session, "wsi", choices= unique(isolate(obs())$wigos_stations_identifier))
    }else{
      updateSelectInput(session, "wsi", choices= NULL)
    }
  })

  # Observe selected Wis2 box and update topic choices accordingly
  observe({
    if(endpoint()!=""){
      message("Updating list of collections / topics")
      message(endpoint())
      url_ <- paste0(
        isolate(input$endpoint),
        "collections/discovery-metadata/items?f=json"
      )
      thedata <- fetch_data(url_ )
      if( thedata$status == 200){
        updateSelectInput(session, "topic",
                          choices = thedata$payload$identifier,
                          selected = thedata$payload$identifier[1])
        thedata$payload
      }else{
        NULL
      }
    }
  })

  metadata <- reactive({
    if( endpoint() != "" & topic() !=""){
      message("Fetching metadata for collection ...")
      url_ <- paste0(
        input$endpoint,
        "collections/discovery-metadata/items?f=json&id=",
        topic()
      )
      message(url_)
      thedata <- fetch_data(url_)
      if(thedata$status==200){
        thedata$payload
      }else{
        NULL
      }
    }else{
      NULL
    }
  })


  # borders <- geojson_sf(url("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"))




  # generate base map
  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      #addPolygons(data=borders, stroke=FALSE, fillColor = "#e6e6e6", fillOpacity = 1, group="land") %>%
      addScaleBar(position = "bottomleft") %>%
      setView(lat = 46.223551, lng = 6.146139, zoom = 4)
    m
  })


  observe({
    if(!is.null(metadata())){
      message("Adding bounding box")
      # add bounding box
      if(input$showbbox){
        leafletProxy("map") %>% clearGroup("topics")  %>%
          addPolygons(data=metadata(), fillColor="red", fillOpacity = 0.2, group="topics", stroke=FALSE)
      }else{
        leafletProxy("map") %>% clearGroup("topics")
      }
    }
  })


  locations <- reactive({
    if(!is.null(obs())){
      xy <- st_coordinates(obs())
      xy <- data.frame(id=obs()$wigos_station_id, x = xy[,1], y = xy[,2])
      xy$popup <- paste0('<h3><a href="',
                         oscarapi,xy$id,'">',xy$id,'</a></h3>')
      unique(xy)
    }else{
      locations <- NULL
    }
  })


  observe({
    if(endpoint()!="" & topic() != "" ){
      date_range <- c(start_time(), end_time())
      date_range <- format.POSIXct(date_range,"%Y-%m-%dT%H:%M:00Z")
      date_range <- paste(date_range, collapse = "/")
      url_ <- paste0(
        isolate(input$endpoint),
        "collections/",topic(),"/items?datetime=",date_range,"&name=pressure_reduced_to_mean_sea_level"
      )
      if(!is.null(locations())){
        m <- leafletProxy("map") %>% clearGroup("obs")
        if(nrow(locations() > 0)){
          locs <- locations()
          message("Adding observations to map")
          xmid <- mean(locs$x)
          ymid <- mean(locs$y)
          # Updating map
          m <- m %>% setView(lat = ymid, lng = xmid, zoom = 5) %>%
            addCircles(lat=locs$y, lng=locs$x, group="obs", radius = 15E3,
                       weight=1, color="black", fillColor = "red", fillOpacity=0.8, popup=locs$popup ) %>%
            addCircles(lat=locs$y, lng=locs$x, group="obs", radius = 1E3,
                       weight=1, color="black", fillColor = "red", fillOpacity=0.8 )
        }
        m
      }else{
        message("No observations to plot")
      }
    }
  })

  observe({
    if(!is.null(obs())){
      output$datatable <- renderDataTable(obs()[,parameters])
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

