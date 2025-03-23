library(shiny)
library(plotly)
library(shinyWidgets)

# Sample Data: Define GNSS stations
all_station_names <- c("Station A", "Station B", "Station C", "Station D", "Station E")
init_selected_stations <- c("Station A", "Station C")  # Initially selected stations

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
  plotlyOutput("plot")
)

server <- function(input, output, session) {

  trace_count <- reactiveVal(0)  # Keep track of number of arrows

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

    plot_geo() %>%
      layout(geo = g, showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)")
  })

  # 2. Update Arrows Based on Selected Stations
  observeEvent(input$selected_stations, {
    fig_proxy <- plotlyProxy("plot", session)  # Modify existing plot

    # Remove previous arrows if any exist
    if (trace_count() > 0) {
      plotlyProxyInvoke(fig_proxy, "deleteTraces", seq_len(trace_count()))
      trace_count(0)  # Reset count
    }

    # Get selected stations
    selected_data <- subset(segment_data, station %in% input$selected_stations)

    # If no stations selected, stop here (removes all arrows)
    if (nrow(selected_data) == 0) return()

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

    trace_count(nrow(selected_data))  # Update number of arrows added
  })
}

shinyApp(ui, server)
