#' Fit Linear Models Between Mass and Climate Data
#'
#' This function takes a list of data frames (output from splitting calculate_temp_means)
#' and fits linear models between mass and climate data for each combination of days,
#' returning AIC values and other statistics. The function requires a base model structure
#' that will be updated for each climate window.
#'
#' @param climate_means A LIST of data frames, each containing Bio_Date, Start_Date, End_Date, and Summary_Value columns
#' @param bio_data A data frame containing biological data with a Date column. bio_data must also contain a 'climate' column (can be initialized with zeros).
#' @param basemodel An lm model object that will be updated for each climate window (e.g., lm(Mass ~ climate, data = bio_data)). This argument is required and cannot be NULL.
#' @param mass_col Character string specifying the name of the mass column in bio_data
#' @param date_col Character string specifying the name of the date column in bio_data
#'
#' @return A data frame containing:
#'         - Start_Date: Start date of the climate window
#'         - End_Date: End date of the climate window
#'         - AIC: AIC value for the linear model
#'         - R_squared: R-squared value for the model
#'         - Slope: Slope of the relationship
#'         - P_value: P-value for the slope
#'
#' @examples
#' # Example usage:
#' climate_means <- calculate_temp_means(0:2, bio_data = Mass)
#' Mass$climate <- 0
#' basemodel <- lm(Mass ~ climate, data = Mass)
#' results <- fit_climate_models(climate_means, Mass, basemodel = basemodel)
#'
#' @export
fit_climate_models <- function(climate_means, bio_data, basemodel, mass_col = "Mass", date_col = "Date") {
  # Input validation
  if (!is.list(climate_means) || !is.data.frame(bio_data)) {
    stop("climate_means must be a list and bio_data must be a data frame")
  }
  
  if (missing(basemodel) || is.null(basemodel)) {
    stop("'basemodel' is required and cannot be NULL.")
  }
  if (!inherits(basemodel, "lm")) {
    stop("basemodel must be an lm object")
  }
  
  # Check for climate column
  if (!("climate" %in% names(bio_data))) {
    stop("bio_data must contain a 'climate' column. Initialize it with zeros before creating the basemodel.")
  }
  
  # Initialize results data frame
  results <- data.frame(
    Start_Date = character(),
    End_Date = character(),
    AIC = numeric(),
    R_squared = numeric(),
    Slope = numeric(),
    P_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (window_data in climate_means) {
    # Check required columns in each list element
    required_cols <- c("Bio_Date", "Start_Date", "End_Date", "Summary_Value")
    if (!all(required_cols %in% names(window_data))) {
      next
    }
    if (!all(c(mass_col, date_col) %in% names(bio_data))) {
      next
    }
    
    # Merge with biological data
    plot_data <- merge(
      window_data,
      bio_data,
      by.x = "Bio_Date",
      by.y = date_col,
      all = FALSE
    )
    
    # Skip if not enough data points (at least 3 unique response values)
    if (nrow(plot_data) < 3 || length(unique(plot_data[[mass_col]])) < 3) {
      next
    }
    
    # Update climate variable with Summary_Value
    plot_data$climate <- plot_data$Summary_Value
    # Use update with the new data
    model <- try(update(basemodel, data = plot_data), silent = TRUE)
    
    if (inherits(model, "try-error")) {
      next
    }
    
    model_summary <- summary(model)
    coef_summary <- coef(model_summary)
    
    # Get the climate coefficient (either Summary_Value or climate)
    climate_coef <- if ("Summary_Value" %in% rownames(coef_summary)) {
      "Summary_Value"
    } else if ("climate" %in% rownames(coef_summary)) {
      "climate"
    } else {
      NA
    }
    
    # Extract slope and p-value
    if (!is.na(climate_coef)) {
      slope <- coef_summary[climate_coef, "Estimate"]
      pval <- coef_summary[climate_coef, "Pr(>|t|)"]
    } else {
      slope <- NA
      pval <- NA
    }
    
    # Use the first Start_Date and End_Date from this window
    results <- rbind(results, data.frame(
      Start_Date = as.character(window_data$Start_Date[1]),
      End_Date = as.character(window_data$End_Date[1]),
      AIC = AIC(model),
      R_squared = model_summary$r.squared,
      Slope = slope,
      P_value = pval,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by AIC
  results <- results[order(results$AIC), ]
  
  return(results)
} 