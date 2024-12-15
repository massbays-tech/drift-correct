library(shiny)
library(bslib)
library(dplyr)
library(plotly)

source(here::here('R/funcs.R'))

ui <- page_sidebar(
  title = "Drift Correction Simulator",
  sidebar = sidebar(
    width = 400,
    dateInput("start_date", "Start Date", value = Sys.Date()),
    numericInput("days", "Number of Days", value = 10, min = 1, max = 30),
    numericInput("drift_rate", "Drift Rate", value = 0.1, min = 0, max = 1, step = 0.01),
    numericInput("drift_per", "Percent Drifted", value = 0.5, min = 0, max = 1, step = 0.1),
    actionButton("simulate", "Simulate", class = "btn-primary"),
    tags$hr(),
    uiOutput("selected_times_ui"),
    uiOutput("correct_to_ui"),
    uiOutput("correct_drift_button"),
    uiOutput("reset_button")
  ),
  card(
    card_header("Simulated Parameters"),
    plotlyOutput("time_series_plot")
  )
)

server <- function(input, output) {
  
  selected_points <- reactiveVal(list())
  is_corrected <- reactiveVal(FALSE)
  is_simulated <- reactiveVal(FALSE)
  
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
  
  # Create UI for selected times conditionally
  output$selected_times_ui <- renderUI({
    req(is_simulated())
    verbatimTextOutput("selected_times")
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
    is_corrected(FALSE) 
    is_simulated(TRUE)
    sim_fun(
      start_date = input$start_date,
      days = input$days,
      parameters = "temperature",
      drift_rate = input$drift_rate,
      drift_per = input$drift_per
    )
  })
  
  # Create a reactive value to store the current data (original or corrected)
  current_data <- reactiveVal()
  
  # Update current_data when simulation runs
  observeEvent(simulated_data(), {
    current_data(simulated_data())
  })
  
  # Reset to original data
  observeEvent(input$reset, {
    req(simulated_data())
    current_data(simulated_data())
    selected_points(list())
    is_corrected(FALSE)
  })
  
  # Conditionally render the correct to selection
  output$correct_to_ui <- renderUI({
    if (length(selected_points()) == 2) {
      numericInput('correct_to', 'Correct to', value = 0, min = 0, max = NA, step = 1)
    }
  })
  
  
  # Conditionally render the correct drift button
  output$correct_drift_button <- renderUI({
    if (length(selected_points()) == 2) {
      actionButton("correct_drift", "Correct drift", class = "btn-warning")
    }
  })
  
  # Conditionally render the reset button
  output$reset_button <- renderUI({
    if (is_corrected()) {
      actionButton("reset", "Reset to Original", class = "btn-secondary")
    }
  })
  
  # Add observer for correct_drift button
  observeEvent(input$correct_drift, {
    req(length(selected_points()) == 2)
    data <- simulated_data()
    points <- selected_points()
    correct_to <- input$correct_to

    # correction function
    corrected_data <- correctdrift_fun(data, parameter = 'temperature', correct_to = correct_to, 
                                       drift_start_time = points[[1]], drift_end_time = points[[2]])

    # Update the current data
    current_data(corrected_data)
    is_corrected(TRUE) 
  })
  
  output$time_series_plot <- renderPlotly({
    data <- current_data()
    points <- selected_points()
    
    req(data)

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
