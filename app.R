library(shiny)
library(bslib)
library(dplyr)
library(plotly)

source(here::here('R/funcs.R'))

ui <- page_sidebar(
  title = "Time Series Simulator",
  sidebar = sidebar(
    width = 400,
    dateInput("start_date", "Start Date", value = Sys.Date()),
    numericInput("days", "Number of Days", value = 5, min = 1, max = 30),
    numericInput("drift_rate", "Drift Rate", value = 0.05, min = 0, max = 1, step = 0.01),
    numericInput("drift_per", "Percent Drifted", value = 0.5, min = 0, max = 1, step = 0.1),
    actionButton("simulate", "Simulate", class = "btn-primary"),
    tags$hr(),
    verbatimTextOutput("selected_times")
  ),
  card(
    card_header("Simulated Parameters"),
    plotlyOutput("time_series_plot")
  )
)

server <- function(input, output) {
  
  selected_points <- reactiveVal(list())
  
  observeEvent(event_data("plotly_click"), {
    click_data <- event_data("plotly_click")
    current_points <- selected_points()

    if (length(current_points) >= 2) {
      # Reset if we already have 2 points
      selected_points(list())
    } else {
      selected_points(c(current_points, list(click_data$x)))
    }
  })
  
  output$selected_times <- renderText({
    points <- selected_points()
    if (length(points) == 0) return("Click to select start time")
    if (length(points) == 1) return("Click to select end time")
    sprintf("Selected period:\nFrom: %s\nTo: %s", 
            as.POSIXct(points[[1]], origin="1970-01-01"),
            as.POSIXct(points[[2]], origin="1970-01-01"))
  })
  
  simulated_data <- eventReactive(input$simulate, {
    selected_points(list())
    sim_fun(
      start_date = input$start_date,
      days = input$days,
      parameters = "temperature",
      drift_rate = input$drift_rate,
      drift_per = input$drift_per
    )
  })
  
  output$time_series_plot <- renderPlotly({
    data <- simulated_data()
    points <- selected_points()
    
    p <- plot_ly(data, x = ~timestamp) %>%
      add_trace(y = ~temperature, name = "Temperature", type = "scatter", mode = "lines") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Temperature (C)")
      )

    for (point in points) {
      p <- p %>% add_segments(x = point, xend = point,
                              y = min(data$temperature), yend = max(data$temperature),
                              line = list(color = "red", dash = "dash"),
                              showlegend = FALSE)
    }

    p %>% layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Temperature (C)"),
      clickmode = "event"
    )
    
  })
}

shinyApp(ui, server)
