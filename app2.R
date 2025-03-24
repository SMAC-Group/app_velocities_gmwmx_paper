library(shiny)
library(plotly)
library(shinyWidgets)

# Sample Data: Define GNSS stations
all_station_names <- c("Station A", "Station B", "Station C", "Station D", "Station E")
init_selected_stations <- c("Station A","Station B", "Station C")  # Initially selected stations

# Data: Arrows for each station
segment_data <- data.frame(
  station = all_station_names,
  lon_start = c(-90, -70, 30, 80, 100),
  lat_start = c(40, 20, -10, 15, 20),
  lon_end = c(-80, -60, 40, 90, 110),
  lat_end = c(45, 30, 0, 20, 25),
  color = c("red", "blue", "green", "purple", "orange")
)

ui <- fluidPage(
  pickerInput(
    inputId = "selected_stations",
    label = "Select GNSS stations",
    choices = all_station_names,
    multiple = TRUE,
    selected = init_selected_stations,
    options = pickerOptions(
      actionsBox = TRUE,
      title = "Please select GNSS stations",
      header = "Selected GNSS stations"
    )
  ),
  plotlyOutput("plot", width = "100vw", height = "100vh")
)

server <- function(input, output, session) {

  trace_count <- reactiveVal(-1)  # Keep track of number of arrows

  # 1. Render Base Globe Once
  output$plot <- renderPlotly({
    g <- list(
      scope = "world",
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = "orthographic", scale = 0.8),
      showland = TRUE,
      landcolor = toRGB("gray90"),
      showocean = TRUE,
      oceancolor = toRGB("#99C0DBCC"),
      bgcolor = toRGB("black")
    )

    p = plot_geo() %>%
      layout(geo = g, showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)")
    
    # 
    # 
    # # add traces for all selected stations
    # # Add initial arrows
    # selected_data <- subset(segment_data, station %in% init_selected_stations)
    # for (i in seq_len(nrow(selected_data))) {
    #   p <- p %>%
    #     add_trace(
    #       type = "scattergeo",
    #       mode = "lines",
    #       lon = c(selected_data$lon_start[i], selected_data$lon_end[i], NA),
    #       lat = c(selected_data$lat_start[i], selected_data$lat_end[i], NA),
    #       line = list(color = selected_data$color[i], width = 2),
    #       hoverinfo = "none"
    #     )
    # }
    # 
    # trace_count(nrow(selected_data)-1)  # Update number of arrows added
    p
    
  })
  
  
  
  update_arrows <- function() {
    fig_proxy <- plotlyProxy("plot", session = shiny::getDefaultReactiveDomain())  # Modify existing plot
    
    # Remove previous arrows if any exist
    isolate({
      if (trace_count() > -1) {
        plotlyProxyInvoke(fig_proxy, "deleteTraces", as.list(seq_len(trace_count())))
        trace_count(-1)  # Reset count
      }
    })
    
    # Get selected stations
    selected_data <- subset(segment_data, station %in% input$selected_stations)
    
    # If no stations selected, ensure all traces are removed
    if (nrow(selected_data) == 0) {
      return()  # Simply exit after deleting traces
    }
    
    # Add new arrows
    for (i in seq_len(nrow(selected_data))) {
      plotlyProxyInvoke(fig_proxy, "addTraces", list(
        type = "scattergeo",
        mode = "lines",
        lon = c(selected_data$lon_start[i], selected_data$lon_end[i], NA),
        lat = c(selected_data$lat_start[i], selected_data$lat_end[i], NA),
        line = list(color = selected_data$color[i], width = 2),
        hoverinfo = "none"
      ))
    }
    
    # Update trace count
    trace_count(nrow(selected_data))  
  }
  

  
  observeEvent(input$selected_stations, 
               {
                 update_arrows()
               }
  )
  

  
  

  
  
  
  
}

shinyApp(ui, server)
