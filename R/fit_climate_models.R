#' Fit Linear Models Between Mass and Climate Data
#'
#' This function takes a list of data frames (output from splitting calculate_temp_means)
#' and fits linear models between mass and climate data for each combination of days,
#' returning AIC values and other statistics. The function requires a base model structure
#' that will be updated for each climate window.
#'
#' @param climate_means A LIST of data frames, each containing Bio_Date, Start_Date, End_Date, and Summary_Value columns
#' @param basemodel An lm model object that will be updated for each climate window (e.g., lm(Mass ~ climate, data = bio_data)). This argument is required and cannot be NULL.
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
#' results <- fit_climate_models(climate_means, basemodel = basemodel)
#'
#' @export
fit_climate_models <- function(climate_means, basemodel) {
  # Input validation
  if (!is.list(climate_means)) {
    stop("climate_means must be a list")
  }
  
  if (missing(basemodel) || is.null(basemodel)) {
    stop("'basemodel' is required and cannot be NULL.")
  }
  if (!inherits(basemodel, "lm")) {
    stop("basemodel must be an lm object")
  }
  
  # Extract the model frame from basemodel
  model_data <- model.frame(basemodel)
  response_var <- all.vars(formula(basemodel))[1]
  
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
      results <- rbind(results, data.frame(
        Start_Date = NA_character_,
        End_Date = NA_character_,
        AIC = NA_real_,
        R_squared = NA_real_,
        Slope = NA_real_,
        P_value = NA_real_,
        stringsAsFactors = FALSE
      ))
      next
    }
    
    # Skip if not enough data points (at least 3 unique response values)
    if (nrow(model_data) < 3 || length(unique(model_data[[response_var]])) < 3) {
      results <- rbind(results, data.frame(
        Start_Date = as.character(window_data$Start_Date[1]),
        End_Date = as.character(window_data$End_Date[1]),
        AIC = NA_real_,
        R_squared = NA_real_,
        Slope = NA_real_,
        P_value = NA_real_,
        stringsAsFactors = FALSE
      ))
      next
    }
    
    # Update climate variable with Summary_Value
    model_data$climate <- window_data$Summary_Value
    
    # Try to fit the model and extract statistics
    fit_result <- tryCatch({
      model <- update(basemodel, data = model_data)
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
      list(
        AIC = AIC(model),
        R_squared = model_summary$r.squared,
        Slope = slope,
        P_value = pval
      )
    }, error = function(e) {
      list(
        AIC = NA_real_,
        R_squared = NA_real_,
        Slope = NA_real_,
        P_value = NA_real_
      )
    })
    
    results <- rbind(results, data.frame(
      Start_Date = as.character(window_data$Start_Date[1]),
      End_Date = as.character(window_data$End_Date[1]),
      AIC = fit_result$AIC,
      R_squared = fit_result$R_squared,
      Slope = fit_result$Slope,
      P_value = fit_result$P_value,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by AIC (NAs last)
  results <- results[order(is.na(results$AIC), results$AIC), ]
  
  return(results)
} 