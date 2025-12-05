#' Variable Type Detection System
#'
#' @description
#' Unified system for detecting and classifying variable types.
#' This eliminates scattered type-checking logic across plot functions.
#'
#' @name variable-types
#' @keywords internal
NULL

# =============================================================================
# TYPE DETECTION
# =============================================================================

#' Detect variable type
#'
#' @description
#' Determines the type of a variable for styling purposes.
#'
#' Type hierarchy:
#' - continuous: numeric (not factor)
#' - discrete: factor, character, logical
#' - temporal: Date, POSIXct, POSIXlt
#' - ordered: ordered factor
#'
#' @param data Data frame
#' @param column Character. Column name to check
#' @return Character. One of "continuous", "discrete", "temporal", "ordered"
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   x = 1:10,
#'   y = factor(letters[1:10]),
#'   z = Sys.Date() + 1:10
#' )
#' detect_var_type(data, "x") # "continuous"
#' detect_var_type(data, "y") # "discrete"
#' detect_var_type(data, "z") # "temporal"
#' }
detect_var_type <- function(data, column) {
  if (is.null(column)) {
    return(NULL)
  }

  # Handle internal columns (like ".y", ".density")
  if (startsWith(column, ".")) {
    return("continuous")
  }

  col_data <- data[[column]]

  # Check for custom type attribute
  custom_type <- attr(col_data, "ggforge_type", exact = TRUE)
  if (!is.null(custom_type)) {
    return(custom_type)
  }

  # Type detection hierarchy
  if (is.ordered(col_data)) {
    return("ordered")
  }

  if (is.factor(col_data) || is.character(col_data) || is.logical(col_data)) {
    return("discrete")
  }

  if (inherits(col_data, c("Date", "POSIXct", "POSIXlt"))) {
    return("temporal")
  }

  if (is.numeric(col_data)) {
    return("continuous")
  }

  # Fallback
  return("discrete")
}

#' Check if variable is continuous
#'
#' @description
#' Helper to check if a variable should use continuous styling.
#'
#' @param data Data frame
#' @param column Character. Column name
#' @return Logical
#' @keywords internal
is_continuous_var <- function(data, column) {
  if (is.null(column)) {
    return(FALSE)
  }
  type <- detect_var_type(data, column)
  type %in% c("continuous", "temporal")
}

#' Check if variable is discrete
#'
#' @description
#' Helper to check if a variable should use discrete styling.
#'
#' @param data Data frame
#' @param column Character. Column name
#' @return Logical
#' @keywords internal
is_discrete_var <- function(data, column) {
  if (is.null(column)) {
    return(FALSE)
  }
  type <- detect_var_type(data, column)
  type %in% c("discrete", "ordered")
}

#' Annotate variable with explicit type
#'
#' @description
#' Allows users to override automatic type detection.
#'
#' @param x Vector. The variable to annotate
#' @param type Character. One of "continuous", "discrete", "temporal", "ordered"
#' @return The vector with type attribute attached
#' @export
#'
#' @examples
#' \dontrun{
#' # Force a numeric year column to be treated as discrete
#' data$year <- var_type(data$year, "discrete")
#'
#' # Force a character column to be treated as continuous (rare)
#' data$score <- var_type(as.numeric(data$score), "continuous")
#' }
var_type <- function(x, type = c("continuous", "discrete", "temporal", "ordered")) {
  type <- match.arg(type)
  attr(x, "ggforge_type") <- type
  x
}
