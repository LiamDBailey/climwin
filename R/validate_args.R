#' Validate Function Arguments
#'
#' This function checks if an argument is present and of the correct type.
#'
#' @param arg_name Character string. Name of the argument to check.
#' @param arg_value The value of the argument to check.
#' @param required Logical. Whether the argument is required.
#' @param type Character string or vector of character strings. The expected type(s) of the argument.
#' @param additional_checks Optional function or list of functions that perform additional validation checks.
#'                         Each function should return TRUE if valid, FALSE or error message if invalid.
#'                         If a list is provided, all checks must pass for validation to succeed.
#'
#' @return TRUE if all checks pass, otherwise stops with an error message.
#'
#' @examples
#' \dontrun{
#' # Check required numeric argument
#' validate_arg("range", 1:10, required = TRUE, type = "numeric")
#'
#' # Check optional character argument with additional validation
#' validate_arg("type", "relative", required = FALSE, type = "character",
#'             additional_checks = function(x) x %in% c("relative", "absolute"))
#'
#' # Check with multiple additional validations
#' validate_arg("value", 5, required = TRUE, type = "numeric",
#'             additional_checks = list(
#'               function(x) if(!x > 0) stop("must be positive"),
#'               function(x) if(!x < 10) stop("must be less than 10")
#'             ))
#' }
#' @export
validate_arg <- function(arg_name, arg_value, required = TRUE, type = NULL, additional_checks = NULL) {
  
  # If argument is required, throw error if it's missing
  if (required && missing(arg_value)) {
    stop(sprintf("'%s' is required", arg_name))
  }
  
  # If argument is not missing, check its type using inherits
  if (!missing(arg_value) && !is.null(type)) {
    if (!any(sapply(type, function(t) inherits(arg_value, t)))) {
      stop(sprintf("'%s' must be of type %s", arg_name, paste(type, collapse = " or ")))
    }
  }
  
  # Run additional checks if provided
  if (!missing(arg_value) && !is.null(additional_checks)) {
    
    # Convert single function to list for consistent handling
    if (is.function(additional_checks)) {
      additional_checks <- list(additional_checks)
    }
    
    # Apply each check function
    for (check in additional_checks){
      tryCatch(check(arg_value),
               error = \(e){
                 stop(sprintf("'%s': %s", arg_name, e$message))
               })
    }
  }
  
  return(TRUE)
}

#' Validate Multiple Function Arguments
#'
#' This function applies the same validation rules to multiple arguments. Useful when
#' multiple arguments need the same validation checks (e.g., multiple data frames).
#'
#' @param args Named list of arguments to validate. Names should match the argument names.
#' @param required Logical. Whether the arguments are required.
#' @param type Character string or vector of character strings. The expected type(s) of the arguments.
#' @param additional_checks Optional function or list of functions that perform additional validation checks.
#'                         Each function should return TRUE if valid, FALSE or error message if invalid.
#'                         If a list is provided, all checks must pass for validation to succeed.
#'
#' @return TRUE if all checks pass, otherwise stops with an error message.
#'
#' @examples
#' # Validate multiple data frames
#' validate_args(
#'   args = list(
#'     climate_data = climate_data,
#'     bio_data = bio_data
#'   ),
#'   required = TRUE,
#'   type = "data.frame",
#'   additional_checks = function(x) if(nrow(x) == 0) stop("must contain at least 1 row")
#' )
#'
#' # Validate multiple character arguments
#' validate_args(
#'   args = list(
#'     cdate = "Date",
#'     bdate = "Date"
#'   ),
#'   required = FALSE,
#'   type = "character"
#' )
#'
#' @export
validate_args <- function(args, required = TRUE, type = NULL, additional_checks = NULL) {
  # Validate args is a named list
  if (!is.list(args) || is.null(names(args)) || any(names(args) == "")) {
    stop("args must be a named list")
  }
  
  # Apply validation to each argument
  for (arg_name in names(args)) {
    validate_arg(
      arg_name = arg_name,
      arg_value = args[[arg_name]],
      required = required,
      type = type,
      additional_checks = additional_checks
    )
  }
  
  return(TRUE)
} 