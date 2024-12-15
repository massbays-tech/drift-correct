sim_fun <- function(
    start_date = Sys.Date(), 
    days = 5, 
    parameters = c("temperature", "pH", "conductivity", "dissolved_oxygen"),
    drift_rate = 0.1, 
    drift_per = 0.5
) {

  # Calculate total hours
  total_hours <- days * 24
  
  # Base parameters with realistic ranges
  base_params <- list(
    temperature = list(mean = 20, sd = 2, min = 0, max = 35),
    pH = list(mean = 7.5, sd = 0.3, min = 6.5, max = 8.5),
    conductivity = list(mean = 500, sd = 50, min = 100, max = 1000),
    dissolved_oxygen = list(mean = 8, sd = 1, min = 0, max = 15)
  )

  # Create time sequence
  time_series <- data.frame(
    timestamp = seq(from = as.POSIXct(start_date, tz = 'UTC'), 
                    by = "hour", 
                    length.out = total_hours)
  )
  
  # Generate simulated data for each parameter
  for (param in parameters) {
    # Extract parameter specifications
    param_spec <- base_params[[param]]
    
    # Initialize base values with some randomness
    values <- rnorm(total_hours, 
                    mean = param_spec$mean, 
                    sd = param_spec$sd)

    # Simulate drift (gradual changing trend)
    ndrift <- floor(drift_per * total_hours / 2)
    midpt <- floor(total_hours / 2)
    drifti <- c(midpt - ndrift + 1, midpt + ndrift)
    drifti <- drifti[1]:drifti[2]
    drift <- seq(0, drift_rate * length(drifti), length.out = length(drifti))
    values[drifti] <- values[drifti] + sample(c(-1, 1), 1) * drift

    # Add to time series
    time_series[[param]] <- values
    
  }
  
  return(time_series)
  
}

correctdrift_fun <- function(
    time_series_data,  # Input dataframe with timestamp and parameter columns
    parameter,         # Name of the parameter to correct
    correct_to,        # Value to correct to
    drift_start_time,  # Start time of the drifted period
    drift_end_time,    # End time of the drifted period
    plot_results = FALSE
) {
  # Subset the data for the entire time series and drift period
  
  drift_start_time <- as.POSIXct(drift_start_time, tz = attr(time_series_data$timestamp, 'tz'))
  drift_end_time <- as.POSIXct(drift_end_time, tz = attr(time_series_data$timestamp, 'tz'))
  
  full_data <- time_series_data
  drift_data <- time_series_data %>%
    mutate(
      ind = 1:n()
    ) |> 
    filter(timestamp >= drift_start_time & timestamp <= drift_end_time)
  
  drift_data <- drift_data |> 
    mutate(
      corrected_value = drift_data[[parameter]] + (correct_to - drift_data[[parameter]][ind == max(ind)]) * 
        (ind - min(ind)) / (max(ind) - min(ind))
    )
  
  # replace the drifted values with corrected values
  corrected_data <- full_data
  corrected_data[corrected_data$timestamp >= drift_start_time & corrected_data$timestamp <= drift_end_time, parameter] <- drift_data$corrected_value
      
  # Optional plotting
  if (plot_results) {
    start_value <- drift_data[[parameter]][drift_data$ind == min(drift_data$ind)]
    final_end_value <- drift_data[[parameter]][drift_data$ind == max(drift_data$ind)]
    
    par(mfrow = c(2,1), mar = c(4,4,2,1))
    
    # Original data
    plot(full_data$timestamp, full_data[[parameter]], 
         type = 'l', 
         main = paste("Original", parameter),
         xlab = "Timestamp", 
         ylab = parameter)
    points(drift_start_time, start_value, col = 'red', pch = 16)
    points(clean_start_time, final_end_value, col = 'red', pch = 16)
    
    # Corrected data
    plot(corrected_data$timestamp, corrected_data[[parameter]], 
         type = 'l', 
         main = paste("Drift Corrected", parameter),
         xlab = "Timestamp", 
         ylab = paste(parameter, "Corrected"))
    # points(drift_start_time, start_value, col = 'red', pch = 16)
    # points(clean_start_time, final_end_value, col = 'red', pch = 16)
  }
  
  return(corrected_data)
  
}
