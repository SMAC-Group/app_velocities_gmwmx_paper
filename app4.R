

library(shiny)

ui <- fluidPage(
  actionButton("update", "Test"),
  plotlyOutput("graphe")
)

server <- function(input, output, session) {
  
  output$graphe <- renderPlotly({
    plot_ly() %>%
      layout(title="test") %>%
      add_trace(x=runif(2), y=runif(2), name="ABC_test", type="scatter", mode="lines+markers")
  })
  
  observeEvent(input$update, {
    plotlyProxy("graphe", session, FALSE) %>%
      plotlyProxyInvoke("deleteTraces", list(as.integer(0))) %>%
      plotlyProxyInvoke("addTraces", list(x=runif(2),
                                          y=runif(2),
                                          name="ABC_test",
                                          type = 'scatter',
                                          mode = 'lines+markers'))
  })
  
}

shinyApp(ui, server)

