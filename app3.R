# library("shiny")
# library("plotly")
# 
# ui <- fluidPage(
#   selectInput("dataset", "Choose a dataset:", choices = c("rock", "mtcars")),
#   
#   plotlyOutput("Plot1")
# )
# 
# 
# server <- function(input, output, session) {
#   
#   dataSource <- reactive({switch(input$dataset,"rock" = rock,"mtcars" = mtcars)})
#   
#   output$Plot1 <-  renderPlotly({plot_ly(data = rock, x = ~area, 
#                                          y =~peri, mode = 'markers', type = 'scatter')})
#   
#   observeEvent(input$dataset, {
#     f <- list(
#       family = "Courier New, monospace",
#       size = 18,
#       color = "#7f7f7f"
#     )
#     x <- list(
#       title = "x Axis",
#       titlefont = f, 
#       range = c(0, 1000)
#     )
#     y <- list(
#       title = "y Axis",
#       titlefont = f,
#       range = c(0, 100)
#     )
#     plotlyProxy("Plot1", session) %>%
#       plotlyProxyInvoke("addTraces", list(x = dataSource()[,1], 
#                                           y = dataSource()[,2],
#                                           type = 'scatter',
#                                           mode = 'markers')) %>% 
#       plotlyProxyInvoke("deleteTraces", list(as.integer(0))) %>% 
#       plotlyProxyInvoke("relayout", list(xaxis = x, yaxis = y))
#   })
#   
#   
#   
# }
# 
# shinyApp(ui, server)