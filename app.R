rm(list=ls())


# devtools::install_github('ropensci/plotly')
library(plotly)
library(maps)
library(RColorBrewer)
library(shiny)

# Function to create column color on df
make_transparent <- function(colors, alpha = 0.5) {
  # Ensure alpha is between 0 and 1
  if (alpha < 0 || alpha > 1) {
    stop("Alpha value must be between 0 and 1")
  }
  
  # Convert colors to RGB and add alpha
  transparent_colors <- sapply(colors, function(col) {
    rgb_val <- col2rgb(col) / 255
    rgb(rgb_val[1], rgb_val[2], rgb_val[3], alpha = alpha)
  })
  
  return(transparent_colors)
}

# Function to get color for a given value
get_color <- function(value, breaks, palette) {
  if (value < breaks[1]) {
    return(palette[1])
  }
  if (value >= breaks[length(breaks)]) {
    return(palette[length(palette)])
  }
  idx <- findInterval(value, breaks)
  return(palette[idx])
}

calculate_arrows = function(vectors, arrow_length = 0.5, arrow_width = 0.2) {
  A = cbind(vectors$lon, vectors$lat)
  B = cbind(vectors$lonend, vectors$latend)
  
  # normalized direction vector as a unit vector
  v = B - A
  w = v/sqrt(rowSums(v^2))
  
  # dealing with lon/lat, we need to account for latitude scaling
  # perpendicular vector needs to be scaled by cos(latitude) for longitude component
  lat_rad = B[,2] * pi/180  # Convert latitude to radians
  scale_factor = cos(lat_rad)
  # perpendicular vector with latitude scaling
  u = cbind(-w[,2]/scale_factor, w[,1]*scale_factor)
  
  # arrow points
  S = B - arrow_width * u
  T = B + arrow_width * u
  P = B + arrow_length * w
  
  arrow_data = data.frame(
    lon = c(rbind(S[,1], T[,1], P[,1], S[,1])),
    lat = c(rbind(S[,2], T[,2], P[,2], S[,2])),
    group = rep(1:nrow(vectors), each = 4)
  )
  return(arrow_data)
}


load_df_vel = function(filename, scale_arrow = .15, arrow_width=0.2, arrow_len=0.5){
  
  
  
  
  # filename="merge_df.rda"
  # scale_arrow = 150
  # arrow_width=0.2
  # arrow_len=0.5
  # 
  
  
  
  
  
  load(filename)
  set.seed(123)
  names(df_merge2)
  df_velocities_gmwmx_and_pbo = df_merge2[sample(1:1000, 100), ]
  # names(df_velocities_gmwmx_and_pbo)
  
  df_velocities_gmwmx_and_pbo$uncertainty_NE <- sqrt(df_velocities_gmwmx_and_pbo$std_trend_gmwmx_dN_scaled^2 + df_velocities_gmwmx_and_pbo$std_trend_gmwmx_dE_scaled^2)

  
  x0 = df_velocities_gmwmx_and_pbo$longitude
  y0 = df_velocities_gmwmx_and_pbo$latitude
  x1 <- as.numeric(df_velocities_gmwmx_and_pbo$longitude + df_velocities_gmwmx_and_pbo$trend_gmwmx_dE_scaled * scale_arrow)
  y1 <- as.numeric(df_velocities_gmwmx_and_pbo$latitude + df_velocities_gmwmx_and_pbo$trend_gmwmx_dN_scaled * scale_arrow)
  
  vectors = data.frame(
    name = df_velocities_gmwmx_and_pbo$station_name,
    lat = y0,
    lon = x0,
    latend = y1,
    lonend = x1,
    horiz_N = df_velocities_gmwmx_and_pbo$trend_gmwmx_dN_scaled,
    horiz_N_sd = df_velocities_gmwmx_and_pbo$std_trend_gmwmx_dN_scaled,
    horiz_E = df_velocities_gmwmx_and_pbo$trend_gmwmx_dE_scaled,
    horiz_E_sd = df_velocities_gmwmx_and_pbo$std_trend_gmwmx_dE_scaled,
    height = df_velocities_gmwmx_and_pbo$trend_gmwmx_dU_scaled,
    height_sd = df_velocities_gmwmx_and_pbo$std_trend_gmwmx_dU_scaled
  )
  
  # NE colors
  breaks_col <- unique(quantile(df_velocities_gmwmx_and_pbo$uncertainty_NE))
  my_green_to_red <- colorRampPalette(c("#FFA500", "#FF8C00", "#FF6347","red", "#8B0000"))
  n_colors <- length(breaks_col) - 1
  palette_breaks <- my_green_to_red(n_colors)
  df_velocities_gmwmx_and_pbo$color_std_NE <- make_transparent(sapply(df_velocities_gmwmx_and_pbo$uncertainty_NE, 
                                                                      get_color, 
                                                                      breaks = breaks_col, 
                                                                      palette = palette_breaks),
                                                               alpha = .5)        
  # Up/Down negative colors
  vectors$height_col = NA
  U_ind_neg = vectors$height<0
  breaks_col <- unique(quantile(vectors$height[U_ind_neg]))
  # my_blues <- colorRampPalette(c("#E6F3FF", "#94C7E1", "#6CB0D2", "#0066A5"))
  # my_blues <- colorRampPalette(c("#6CB0D2", "#0066A5", "#004C7A", "#003357"))
  my_blues <- colorRampPalette(c("#FFCCCC", "#FF6666", "#CC0000", "#990000"))
  n_colors <- length(breaks_col) - 1
  palette_breaks <- my_blues(n_colors)
  vectors$height_col[U_ind_neg] <- make_transparent(sapply(vectors$height[U_ind_neg], 
                                                           get_color, 
                                                           breaks = breaks_col, 
                                                           palette = palette_breaks),
                                                    alpha = .5)        
  # Up/Down positive colors
  U_ind_pos = vectors$height>0
  breaks_col <- unique(quantile(vectors$height[U_ind_pos]))
  my_greens <- colorRampPalette(c("#E6FFE6", "#B3E6B3", "#80CC80", "#4DB34D"))
  # my_greens <- colorRampPalette(c("#80CC80", "#4DB34D", "#2E8B2E", "#1F661F"))
  n_colors <- length(breaks_col) - 1
  palette_breaks <- my_greens(n_colors)
  vectors$height_col[U_ind_pos] <- make_transparent(sapply(vectors$height[U_ind_pos], 
                                                           get_color, 
                                                           breaks = breaks_col, 
                                                           palette = palette_breaks),
                                                    alpha = .5)        
  
  arrow_coords = calculate_arrows(vectors, arrow_len, arrow_width)
  arrow_coords$cols_NE = rep(df_velocities_gmwmx_and_pbo$color_std_NE, each=4)
  vectors$cols_NE = df_velocities_gmwmx_and_pbo$color_std_NE
  
  return(list(vectors = vectors, arrow_coords = arrow_coords))
}

ui <- fluidPage(
  tags$head(
    tags$script("
      window.onload = function() {
        window.scrollTo((document.documentElement.scrollWidth - document.documentElement.clientWidth) / 2, 
                       document.body.scrollHeight / 4);
      }
    "),
    tags$style(HTML("
      body { 
        background-color: black !important;
      }
      .control-panel {
        position: fixed;
        bottom: 0;
        left: 0;
        right: 0;
        width: 100%;
        background-color: rgba(20, 20, 20, 0.9);
        padding: 8px;
        z-index: 1000;
        display: flex;
        align-items: center;
      }
      .control-group {
        display: flex;
        flex-direction: column;
        gap: 2px;
        align-items: flex-start;
        padding-left: 15px;
      }
      .control-panel .checkbox {
        margin: 0;
        line-height: 1;
      }
      .control-panel .checkbox label {
        color: white;
        font-size: 12px;
        padding: 0;
        display: flex;
        align-items: center;
        gap: 5px;
      }
      .control-panel input[type='checkbox'] {
        -webkit-appearance: none;
        appearance: none;
        width: 12px;
        height: 12px;
        margin: 0;
        background: white;
        border: 1px solid #ccc;
        border-radius: 2px;
        position: relative;
        cursor: pointer;
      }
      .control-panel input[type='checkbox']:checked {
        background: #2196F3;
        border-color: #2196F3;
      }
      .control-panel input[type='checkbox']:checked:after {
        content: '';
        position: absolute;
        left: 3px;
        top: 0px;
        width: 4px;
        height: 8px;
        border: solid white;
        border-width: 0 2px 2px 0;
        transform: rotate(45deg);
      }
      .legend-container {
        display: flex;
        flex-direction: column;
        align-items: flex-end;
        width: 100%;
        padding-right: 15px;
      }
      .legend-content {
        display: flex;
        flex-direction: column;
        align-items: center;
      }
      .legend-main-title {
        color: white;
        font-size: 12px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 4px;
        width: 120px;
      }
      .legend-columns {
        display: flex;
        flex-direction: row;
        gap: 20px;
        align-items: flex-start;
      }
      .legend-column {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 4px;
      }
      .legend-title {
        color: white;
        font-size: 11px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 2px;
      }
      .color-box {
        width: 120px;
        height: 12px;
        border: 1px solid rgba(255, 255, 255, 0.7);
        border-radius: 2px;
      }
      .scale-labels {
        width: 120px;
        display: flex;
        justify-content: space-between;
        color: white;
        font-size: 10px;
        margin-top: 2px;
      }
      .up-gradient {
        background: linear-gradient(to right,
          rgba(230,255,230,0.5),
          rgba(179,230,179,0.5),
          rgba(128,204,128,0.5),
          rgba(77,179,77,0.5)
        );
      }
      .down-gradient {
        background: linear-gradient(to right,
          rgba(255,204,204,0.5),
          rgba(255,102,102,0.5),
          rgba(204,0,0,0.5),
          rgba(153,0,0,0.5)
        );
      }
      .uncertainty-gradient {
        background: linear-gradient(to right,
          rgba(255,165,0,0.5),
          rgba(255,140,0,0.5),
          rgba(255,99,71,0.5),
          rgba(255,0,0,0.5),
          rgba(139,0,0,0.5)
        );
      }
      .container-fluid {
        width: 100%;
        max-width: none;
        padding: 0;
      }
    "))
  ),
  
  fluidRow(
    class = "control-panel",
    column(6, style="padding-left: 0;",
           div(class = "control-group",
               checkboxInput("show_arrows", "Show estimated tectonic velocities", TRUE),
               checkboxInput("show_bars", "Show estimated crustal uplfit velocity", FALSE)
            
           )
    ),
    column(6,
           div(class = "legend-container",
               div(class = "legend-content",
                   div(class = "legend-main-title", "Legend"),
                   div(class = "legend-columns",
                       div(class = "legend-column",
                           div(class = "legend-title", "Vertical (up)"),
                           div(class = "color-box up-gradient"),
                           div(class = "scale-labels",
                               span("Low"),
                               span("High")
                           )
                       ),
                       div(class = "legend-column",
                           div(class = "legend-title", "Vertical (down)"),
                           div(class = "color-box down-gradient"),
                           div(class = "scale-labels",
                               span("Low"),
                               span("High")
                           )
                       ),
                       div(class = "legend-column",
                           div(class = "legend-title", "Horizontal Uncertainty"),
                           div(class = "color-box uncertainty-gradient"),
                           div(class = "scale-labels",
                               span("Low"),
                               span("High")
                           )
                       )
                   )
               )
           )
    )
  ),
  
  plotlyOutput("plot", width = "150vw", height = "206vh")
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    
    bar_width = 12
    bar_scale = .2
    scale_arrow = .15
    arrow_len = 0.5
    arrow_width = 0.2
    
    df = load_df_vel(filename="merge_df.rda", scale_arrow = scale_arrow, arrow_width=arrow_width, arrow_len=arrow_len)
    vectors = df$vectors
    arrow_coords = df$arrow_coords
    
    g = list(
      scope = 'world',
      showframe = FALSE,
      showcoastlines = T,
      coastlinewidth = 1,
      projection = list(type = 'orthographic',
                        scale = 0.45,
                        resolution = '100'), 
      resolution = "10", # 50
      showcountries = F,
      # countrycolor = '#d1d1d1',
      showland = TRUE,
      landcolor = toRGB("gray90"), # toRGB("#e5ecf6"),
      showocean = TRUE,
      oceancolor = toRGB("#99C0DBCC"), # '#99c0db', # 99C0DBCC
      showlakes = F,
      lakecolor = toRGB("#99C0DBCC"),
      showrivers = F,
      rivercolor = toRGB('#99C0DBCC'),
      # fitbounds = "locations",
      bgcolor = toRGB("black") 
    )
    
    fig = plot_geo()
    
    if(input$show_arrows) {
      for (col_i in unique(vectors$cols_NE)){
        fig = fig %>%
          add_segments(
            data = vectors[vectors$cols_NE==col_i,],
            x = ~lon, xend = ~lonend,
            y = ~lat, yend = ~latend,
            colors = col_i,
            mode = "lines",
            line = list(color = col_i, width = 2),
            hoverinfo = "none"
          ) %>% 
          add_trace(
            data = arrow_coords[arrow_coords$cols_NE==col_i,],
            type = "scattergeo",
            lon = ~lon,
            lat = ~lat,
            split = ~group,
            mode = "lines",
            fill = "toself",
            fillcolor = col_i, #~cols_NE,
            line = list(color = col_i),
            showlegend = FALSE,
            hoverinfo = "none"
          )
      }
    }
    
    if(input$show_bars) {
      for (col_i in unique(vectors$height_col)){
        fig = fig %>%
          add_segments(
            data = vectors[vectors$height_col==col_i,],
            x = ~lon, xend = ~lon,
            y = ~lat, yend = ~lat + height * bar_scale,
            line = list(color = col_i, width = bar_width),
            hoverinfo = "none"
          ) 
      }
    }
    
    fig = fig %>%
      add_trace(
        data = vectors,
        type = "scattergeo",
        lon = ~lon,
        lat = ~lat,
        mode = "markers",
        # marker = list(size = 5, color = ~cols_NE),
        marker = list(size = 2.5, color = "black"),
        # hoverinfo = "none"
        hoverinfo = "text",
        text = ~paste0(
          "Station ID: ", name,
          "<br>──────────────────",
          "<br>Latitude:  ", round(lat, 2), "°",
          "<br>Longitude: ", round(lon, 2), "°",
          "<br>──────────────────",
          "<br>            Velocity (std. dev) mm/year",
          "<br>North/South: ", sprintf("%8.5f", horiz_N), " (", sprintf("%.5f", horiz_N_sd), ")",
          "<br>East/West:   ", sprintf("%8.5f", horiz_E), " (", sprintf("%.5f", horiz_E_sd), ")",
          "<br>Up/Down:     ", sprintf("%8.5f", height), " (", sprintf("%.5f", height_sd), ")"
        ),
        # text <- ~paste0(
        #   "<table style='width:100%'>",
        #   "<tr><td style='text-align:left;'>Station ID:</td><td style='text-align:right;'>", name, "</td></tr>",
        #   "<tr><td style='text-align:left;'>Latitude:</td><td style='text-align:right;'>", round(lat, 2), "°</td></tr>",
        #   "<tr><td style='text-align:left;'>Longitude:</td><td style='text-align:right;'>", round(lon, 2), "°</td></tr>",
        #   "</table>"
        # ),
        
        
        hoverlabel = list(
          bgcolor = "white",
          bordercolor = "black",
          font = list(
            family = "monospace",
            size = 12,
            color = "black"
          )
        )
      )
    
    # fig$sizingPolicy$padding <- "0"
    fig = fig %>% layout(showlegend = FALSE, geo = g,
                         # autosize=F,
                         paper_bgcolor='rgba(0,0,0,0)',
                         plot_bgcolor='rgba(0,0,0,0)',
                         margin = list(l = 0, r = 0, t = 0, b = 0, pad = 0, autoexpand = T))
    
  })
}

shinyApp(ui, server)

