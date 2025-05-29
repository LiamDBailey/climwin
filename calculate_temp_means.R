# Function to calculate mean temperatures for different date ranges
calculate_temp_means <- function(range) {
  # Read the climate data
  climate_data <- read.csv("MassClimate.csv")
  
  # Convert Date column to Date format
  climate_data$Date <- as.Date(climate_data$Date, format = "%d/%m/%Y")
  
  # Create a reference date (01/01/1979)
  reference_date <- as.Date("1979-01-01")
  
  # Initialize empty vectors to store results
  start_dates <- character()
  end_dates <- character()
  mean_temps <- numeric()
  
  # Loop through each possible range
  for (i in range) {
    # Calculate start date (i days before reference date)
    start_date <- reference_date - i
    end_date <- reference_date
    
    # Filter data for the date range
    date_range_data <- climate_data[climate_data$Date >= start_date & 
                                   climate_data$Date <= end_date, ]
    
    # Calculate mean temperature for this range
    mean_temp <- mean(date_range_data$Temp)
    
    # Store results
    start_dates <- c(start_dates, as.character(start_date))
    end_dates <- c(end_dates, as.character(end_date))
    mean_temps <- c(mean_temps, mean_temp)
  }
  
  # Create and return results dataframe
  results <- data.frame(
    Start_Date = start_dates,
    End_Date = end_dates,
    Mean_Temperature = mean_temps
  )
  
  return(results)
}

# Example usage:
# result <- calculate_temp_means(0:100)
# print(result) 