# load libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(geojsonsf)
library(sf)
library(jsonlite)
library(geosphere)

# set timezone to use
Sys.setenv(TZ='UTC')

DATA_DIR <- "./data/HINNAMNOR/"

# Function to create polygons from TC geojson
make_polygon <- function(feature){
  # check feature is a radius
  if( feature$properties$name == "effective_radius_with_respect_to_wind_speeds_above_threshold"){
    bearings <- subset(feature$properties$metadata, name == "bearing_or_azimuth")
    bearings <- unlist(bearings$value)
    if(bearings[2] == 0)bearings[2] = 360
    geom <- feature$geometry$coordinates
    geom <- rbind( geom, destPoint(geom, bearings[1]:bearings[2], feature$properties$value), geom)
    names(geom) <- NULL
    feature$geometry$type <- "Polygon"
    feature$geometry$coordinates <- list(geom)
    idx <- which( feature$properties$metadata$name == "wind_speed_threshold")
    feature$properties$name <- "wind_speed_threshold"
    feature$properties$value <- as.numeric(feature$properties$metadata$value[idx][1])
    feature$properties$units <- feature$properties$metadata$units[idx]
    feature$properties$metadata <- feature$properties$metadata[-idx,]
    idx <- which( feature$properties$metadata$name == "bearing_or_azimuth")
    feature$properties$metadata <- feature$properties$metadata[-idx,]
  }
  return(feature)
}

# Function to load data, currently from disk but will be updated with API call
load_data <- function(system_name, ensemble_member){
    cat("Loading data ...\n", file=stderr())
    pat <- paste0(system_name,"_20220831T000000_",ensemble_member,"_.+json")
    # get list of files to process
    files <- list.files(DATA_DIR,pat)
    if(length(files)==0)return(NULL)
    # read
    datain <- lapply(paste0(DATA_DIR,files), fromJSON)
    # convert polygons
    datain <- lapply( datain, make_polygon)
    # now convert to simple features
    datain <- lapply( datain, FUN = function(X){
        geojson_sf( toJSON(X, auto_unbox = TRUE))
    })
    # form list to data frame
    datain <- do.call("rbind", datain)
    datain$resultTime <- as.POSIXct(datain$resultTime,format = "%Y-%m-%dT%H:%M:%SZ")
    datain <- datain[order(datain$resultTime),]
    return(datain)
}

# get collections
get_collections <- function(){
    collections <- "HINNAMNOR-13W"
}

get_forecast_times <- function(){
    "2022-08-31T00:00Z"
}

####################
# UI look and feel #
####################
ui <- navbarPage("Fiji WIS2 demo", id="nav",
    tabPanel("Map view",
        tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
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
                selectInput("collection","System", get_collections()),
                selectInput("resultTime","Forecast time", get_forecast_times()),
                selectizeInput("ensembleMember","Ensemble members", 0:49),
                h3("Elements to display"),
                #checkboxInput("strike","Strike probability", value=FALSE),
                #checkboxInput("track","Ensemble members", value=FALSE),
                checkboxInput("plots","Time series", value=FALSE),
                #checkboxInput("obs","Recent observations (last 6 hours)", value=FALSE),
                checkboxInput("anim","Animation", value=FALSE),
                checkboxInput("map_labels","Labels on map", value=FALSE)
            ),
        conditionalPanel( "input.plots",
            absolutePanel(
                id="plots_panel", class = "panel panel-default", fixed = FALSE,
                    draggable = TRUE, top = 100, right = 20, left = "auto", bottom = "auto",
                    width = 450, height = "auto",
                    plotOutput("slp_plot", width=400, height=300),
                    plotOutput("wspd_plot", width=400, height=300)
            )
        ),

        conditionalPanel( "input.anim",
            absolutePanel(
                style = "background-color: white; opacity: 0.8",
                bottom = 50, right = "25%", draggable = FALSE, width = "50%",
                div(
                sliderInput("datetime","Select hour",
                            min=as.POSIXct("2022-08-31", format="%Y-%m-%d"),
                            max = as.POSIXct("2022-09-06 18:00", format="%Y-%m-%d %H:%M"),
                            value = as.POSIXct("2022-08-31", format="%Y-%m-%d"),
                            step = 3600*6, width="100%",
                            ticks=FALSE, animate = animationOptions(interval = 250, loop=TRUE),
                            timezone="+0000")
                )
            )
        )

    ),
    tabPanel("Data",
        fluidRow(
            column(12,
                dataTableOutput("data_table")
            )
        )
    ),
    tabPanel("Configuration",
        fluidRow(
            column(4,
                textInput("endpoint","End point", "http://demo.wis2box.wis.wmo.int/oapi"),
                textInput("token","Token","")
            )
        )
    ),
    tabPanel("About"),
    tabPanel("Code")
)

# server logic to draw map
server <- function(input, output, session) {
  # load data
  get_data <- reactive({
    load_data(input$collection,input$ensembleMember)
  }) %>% bindCache(input$collection,input$ensembleMember)
  # datain <- load_data("HINNAMNOR-13W",1)
  # generate base map
  output$map <- renderLeaflet({
    m <- leaflet( options=leafletOptions(zoomControl=FALSE)) %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addScaleBar(position = "bottomleft") %>%
        setView(lat = 30, lng = 127, zoom = 6)
    m
    }
  )

    observe({
        timestep <- input$datetime
        thedata <- get_data()
        proxy <- leafletProxy("map") %>% clearShapes() %>% clearMarkers()
        if( !is.null(thedata) ){
            ss <- subset(thedata, resultTime == input$datetime)
            proxy <- proxy %>% addPolygons(
                data = subset(ss, name== "wind_speed_threshold" & value == 18 ),
                fillColor="green",
                fillOpacity=0.5,
                stroke=FALSE)
            proxy <- proxy %>% addPolygons(
                data = subset(ss, name== "wind_speed_threshold" & value == 26 ),
                fillColor="orange",
                fillOpacity=0.5,
                stroke=FALSE)
            proxy <- proxy %>% addPolygons(
                data = subset(ss, name== "wind_speed_threshold" & value == 33 ),
                fillColor="red",
                fillOpacity=0.5,
                stroke=FALSE)

            proxy <- proxy %>%
                addPolylines(data = st_coordinates(subset(thedata, name== "wind_speed_at10m" )), color="red", opacity=0.5) %>%
                addCircles( data = subset(thedata, name== "wind_speed_at10m" ), radius = 25E3,
                    fillColor="red", fillOpacity=0.5,stroke=FALSE)

            proxy <- proxy %>%
                addLabelOnlyMarkers(
                    data = subset(ss, name == "wind_speed_at10m"),
                    label=format.POSIXct(timestep, "%Y-%m-%d %H:%MZ"),
                    labelOptions = labelOptions(noHide = TRUE, direction = 'bottom', textOnly = TRUE, textsize="16pt",
                            style = list("color" = "white")))
        }
        proxy
    })

  observe({
    thedata <- get_data()
    speeds <- subset(thedata, name== "wind_speed_at10m" )
    pressures <- subset(thedata, name== "pressure_reduced_to_mean_sea_level" )
    proxy <- leafletProxy("map") %>% clearShapes()
    if( !is.null(thedata) ){
        if(input$map_labels){
            proxy <- proxy %>%
                    addLabelOnlyMarkers(data = speeds, label=paste(speeds$value, speeds$units),
                        labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, textsize="16pt",
                            style = list("color" = "white"))) %>%
                    addLabelOnlyMarkers(data = speeds, label=paste(pressures$value, pressures$units),
                        labelOptions = labelOptions(noHide = TRUE, direction = 'bottom', textOnly = TRUE, textsize="16pt",
                            style = list("color" = "white")))
        }else{
            proxy <- proxy %>% clearMarkers()
        }
        proxy <- proxy %>%
            addPolylines(data = st_coordinates(speeds), color="red", opacity=0.5) %>%
            addCircles(data = speeds, radius = 25E3, fillColor="red", fillOpacity=0.5,stroke=FALSE)
    }
    proxy
  })


  observe({
    thedata <- get_data()
    speeds <- subset(thedata, name== "wind_speed_at10m" )
    pressures <- subset(thedata, name== "pressure_reduced_to_mean_sea_level" )
    output$slp_plot <- renderPlot({
        plot(value ~ resultTime, pressures, ylab = "Pressure (hPa)", xlab="", type="l", main="Minimum pressure")
        if(!is.null( input$datetime)){
            abline(v=input$datetime, col="red", lty = 2)
            idx <- which(pressures$resultTime == input$datetime)
            if(length(idx) == 1){
                x <- min(pressures$resultTime, na.rm=TRUE)
                y <- min(pressures$value, na.rm = TRUE) +
                    0.9*(max(pressures$value, na.rm = TRUE) - min(pressures$value, na.rm = TRUE))
                text(x=x,y=y,labels=paste(pressures$value[idx],pressures$units[idx]), pos=4)
            }
        }
    })
    output$wspd_plot <- renderPlot({
        plot(value ~ resultTime, speeds, ylab = "Wind speed (m/s)", ylim=c(0,50), xlab = "", type="l", main="Maximum wind speed")
        if(!is.null( input$datetime)){
            abline(v=input$datetime, col="red", lty = 2)
            idx <- which(speeds$resultTime == input$datetime)
            if(length(idx) == 1){
                x <- min(speeds$resultTime, na.rm=TRUE)
                y <- 45 #min(speeds$value, na.rm = TRUE) +
                    #0.9*(max(speeds$value, na.rm = TRUE) - min(speeds$value, na.rm = TRUE))
                text(x=x,y=y,labels=paste(speeds$value[idx],speeds$units[idx]), pos=4)
            }
        }
    })
  })

  observe({
    thedata <- get_data()
    output$data_table <- renderDataTable(thedata)
  })
}

# Run the application
shinyApp(ui = ui, server = server)