# ============================================================================
# Helper Functions for Text Angle Detection
# ============================================================================

#' Auto-detect optimal x-axis text angle based on label length
#'
#' @description
#' Automatically determines the best angle for x-axis labels based on
#' the maximum label length, number of labels, and plot orientation.
#'
#' @param data A data frame containing the plot data
#' @param x Character string specifying the x-axis column name
#' @param flip Logical; whether the plot coordinates are flipped
#' @param stack Logical; whether facets are stacked
#' @param n_groups Integer; number of groups (for dodged plots)
#'
#' @return Numeric value (0, 45, or 90) representing the optimal text angle
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' df <- data.frame(
#'   short_labels = factor(c("A", "B", "C")),
#'   long_labels = factor(c("Category One", "Category Two", "Category Three"))
#' )
#'
#' # Short labels - returns 0 (horizontal)
#' auto_detect_text_angle(df, "short_labels", flip = FALSE, stack = FALSE)
#'
#' # Long labels - returns 45 (angled)
#' auto_detect_text_angle(df, "long_labels", flip = FALSE, stack = FALSE)
#' }
#'
#' @keywords internal
auto_detect_text_angle <- function(data, x, flip = FALSE, stack = FALSE, n_groups = 1) {
  # Get x-axis labels
  x_labels <- levels(data[[x]])
  if (is.null(x_labels)) {
    x_labels <- unique(as.character(data[[x]]))
  }

  # Calculate max label length
  max_length <- max(nchar(x_labels), na.rm = TRUE)
  n_labels <- length(x_labels)

  # Decision tree for text angle based on label characteristics
  if (flip && stack) {
    if (max_length > 15) {
      return(90)
    } else if (max_length > 8) {
      return(45)
    } else {
      return(0)
    }
  } else if (flip) {
    if (max_length > 20) {
      return(45)
    } else {
      return(0)
    }
  } else {
    if (max_length > 12 || (max_length > 6 && n_labels > 8)) {
      return(45)
    } else if (max_length > 8 && n_labels > 5) {
      return(45)
    } else {
      return(0)
    }
  }
}

# ============================================================================
# Helper Functions for Statistical Comparisons
# ============================================================================

#' Check if a vector has zero variance
#'
#' @description
#' Determines whether a numeric vector has zero variance (all values identical
#' or all NA). Used to prevent statistical test failures.
#'
#' @param vec Numeric vector to check
#'
#' @return Logical; TRUE if vector has zero variance, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' has_zero_variance(c(1, 1, 1, 1))
#' # [1] TRUE
#'
#' has_zero_variance(c(1, 2, 3, 4))
#' # [1] FALSE
#'
#' has_zero_variance(c(NA, NA, NA))
#' # [1] TRUE
#'
#' has_zero_variance(c(5, NA, 5, NA))
#' # [1] TRUE
#' }
#'
#' @keywords internal
has_zero_variance <- function(vec) {
  all(is.na(vec)) || length(unique(vec[!is.na(vec)])) <= 1
}

#' Add minimal variance to vector
#'
#' @description
#' Adds tiny perturbations to a constant vector to enable statistical tests.
#' This is used when data has zero variance but comparisons are still needed.
#'
#' @param yval Numeric vector to modify
#' @param base_value Optional base value for calculating epsilon
#'
#' @return Modified numeric vector with minimal variance added
#'
#' @examples
#'
#' # Constant vector
#' vec <- c(5, 5, 5, 5)
#' add_minimal_variance(vec)
#' # Returns approximately c(4.9999999995, 5.0000000005, 5, 5)
#'
#' # Vector with NA
#' vec_na <- c(10, 10, NA, 10)
#' add_minimal_variance(vec_na)
#' # Adds variance only to non-NA values
#'
#' @keywords internal
add_minimal_variance <- function(yval, base_value = NULL) {
  non_na_idx <- which(!is.na(yval))
  if (length(non_na_idx) < 2) {
    return(yval)
  }

  unique_y <- unique(yval[!is.na(yval)])
  if (length(unique_y) != 1) {
    return(yval)
  }

  # Calculate epsilon relative to data scale
  if (is.null(base_value)) base_value <- unique_y[1]
  epsilon <- max(abs(base_value) * 1e-10, 1e-10)

  yval[non_na_idx[1]] <- unique_y[1] - epsilon
  yval[non_na_idx[2]] <- unique_y[1] + epsilon
  return(yval)
}

#' Check if data groups have variance issues for group_by comparisons
#'
#' @description
#' Examines grouped data to detect zero-variance situations that would
#' cause statistical comparison failures.
#'
#' @param data_groups List of data frames, each representing a group
#' @param x Character string; x-axis column name
#' @param y Character string; y-axis column name
#' @param group_by Character string; grouping column name
#'
#' @return Logical; TRUE if variance issues detected, FALSE otherwise
#'
#' @keywords internal
check_variance_issues_grouped <- function(data_groups, x, y, group_by) {
  for (group_data in data_groups) {
    gs <- unique(as.character(group_data[[group_by]]))
    if (length(gs) >= 2) {
      yval1 <- group_data[[y]][group_data[[group_by]] == gs[1]]
      yval2 <- group_data[[y]][group_data[[group_by]] == gs[2]]
      if (has_zero_variance(yval1) && has_zero_variance(yval2)) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

#' Check if data groups have variance issues for x comparisons
#'
#' @description
#' Examines data groups to detect zero-variance situations for simple
#' (non-grouped) comparisons.
#'
#' @param data_groups List of data frames, each representing a group
#' @param y Character string; y-axis column name
#'
#' @return Logical; TRUE if variance issues detected, FALSE otherwise
#'
#' @keywords internal
check_variance_issues_simple <- function(data_groups, y) {
  for (group_data in data_groups) {
    yval <- group_data[[y]]
    if (has_zero_variance(yval)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Fix variance issues in facet data for grouped comparisons
#'
#' @description
#' Modifies facet data to ensure valid statistical comparisons can be
#' performed when zero-variance groups are detected.
#'
#' @param facet_data Data frame containing facet data
#' @param x Character string; x-axis column name
#' @param y Character string; y-axis column name
#' @param group_by Character string; grouping column name
#'
#' @return Modified data frame with variance issues fixed
#'
#' @keywords internal
fix_facet_variance_grouped <- function(facet_data, x, y, group_by) {
  xdata <- split(facet_data, facet_data[[x]])
  all_gs <- unique(as.character(facet_data[[group_by]]))[1:2]

  for (xval in names(xdata)) {
    df <- xdata[[xval]]
    gs <- unique(as.character(df[[group_by]]))

    if (length(gs) < 2) {
      # Create minimal data for both groups
      new_df <- data.frame(
        x_val = xval,
        y_val = c(0, 1),
        group_val = all_gs,
        stringsAsFactors = FALSE
      )
      colnames(new_df) <- c(x, y, group_by)
      # Preserve factor levels
      if (is.factor(facet_data[[x]])) {
        new_df[[x]] <- factor(new_df[[x]], levels = levels(facet_data[[x]]))
      }
      if (is.factor(facet_data[[group_by]])) {
        new_df[[group_by]] <- factor(new_df[[group_by]], levels = levels(facet_data[[group_by]]))
      }
      xdata[[xval]] <- new_df
      next
    }

    yval1 <- df[[y]][df[[group_by]] == gs[1]]
    yval2 <- df[[y]][df[[group_by]] == gs[2]]

    # Handle all NA cases
    if (all(is.na(yval1))) yval1 <- c(0, rep(NA, length(yval1) - 1))
    if (all(is.na(yval2))) yval2 <- c(1, rep(NA, length(yval2) - 1))

    # Handle zero variance
    if (has_zero_variance(yval1) && has_zero_variance(yval2)) {
      yval1 <- add_minimal_variance(yval1)
      yval2 <- add_minimal_variance(yval2)
    }

    df[[y]][df[[group_by]] == gs[1]] <- yval1
    df[[y]][df[[group_by]] == gs[2]] <- yval2
    xdata[[xval]] <- df
  }

  do.call(rbind, xdata)
}

#' Fix variance issues in facet data for simple comparisons
#'
#' @description
#' Modifies facet data to ensure valid statistical comparisons for
#' non-grouped (simple) comparisons.
#'
#' @param facet_data Data frame containing facet data
#' @param x Character string; x-axis column name
#' @param y Character string; y-axis column name
#'
#' @return Modified data frame with variance issues fixed
#'
#' @keywords internal
fix_facet_variance_simple <- function(facet_data, x, y) {
  xdata <- split(facet_data, facet_data[[x]])

  for (xval in names(xdata)) {
    df <- xdata[[xval]]
    yval <- df[[y]]

    # Handle all NA cases
    if (all(is.na(yval))) {
      yval <- c(0, 1, rep(NA, max(0, length(yval) - 2)))
    }

    # Handle zero variance
    if (has_zero_variance(yval)) {
      yval <- add_minimal_variance(yval)
    }

    df[[y]] <- yval
    xdata[[xval]] <- df
  }

  result <- do.call(rbind, xdata)

  # Preserve factor levels if x was a factor
  if (is.factor(facet_data[[x]])) {
    result[[x]] <- factor(result[[x]], levels = levels(facet_data[[x]]))
  }

  return(result)
}

#' Preprocess data for pairwise comparisons with group_by
#'
#' @description
#' Prepares data for grouped pairwise statistical comparisons by detecting
#' and fixing variance issues that would cause test failures.
#'
#' @param data Data frame containing the plot data
#' @param x Character string; x-axis column name
#' @param y Character string; y-axis column name
#' @param group_by Character string; grouping column name
#' @param facet_by Character vector; faceting column name(s), or NULL
#'
#' @return Preprocessed data frame ready for statistical comparisons
#'
#' @examples
#' \dontrun{
#' # Sample data with potential zero-variance groups
#' df <- data.frame(
#'   treatment = rep(c("A", "B"), each = 10),
#'   response = c(rep(5, 5), rnorm(5, 5), rep(3, 5), rnorm(5, 3)),
#'   group = rep(c("Control", "Test"), 10)
#' )
#'
#' # Preprocess for grouped comparisons
#' df_processed <- preprocess_comparison_data_grouped(
#'   df, "treatment", "response", "group"
#' )
#' }
#'
#' @keywords internal
#' @importFrom dplyr group_by summarise add_count filter mutate
#' @importFrom rlang syms sym
preprocess_comparison_data_grouped <- function(data, x, y, group_by, facet_by = NULL) {
  split_cols <- c(x, y, group_by)
  grouping_vars <- x

  if (!is.null(facet_by)) {
    split_cols <- c(split_cols, facet_by)
    grouping_vars <- c(grouping_vars, facet_by)
  }

  # Create grouping key
  if (length(grouping_vars) > 1) {
    split_key <- interaction(data[grouping_vars], drop = TRUE, sep = " // ")
  } else {
    split_key <- data[[grouping_vars]]
  }

  data_groups <- split(data[, split_cols, drop = FALSE], split_key)
  needs_fix <- check_variance_issues_grouped(data_groups, x, y, group_by)

  if (!needs_fix) {
    return(data)
  }

  warning("Some pairwise comparisons may fail due to insufficient variability. Adjusting data to ensure valid comparisons.")

  # Split by facet if present
  if (!is.null(facet_by)) {
    facet_key <- interaction(data[facet_by], drop = TRUE, sep = " // ")
    facet_splits <- split(data[, split_cols, drop = FALSE], facet_key)
  } else {
    facet_splits <- list(data[, split_cols, drop = FALSE])
  }

  fixed_data_list <- lapply(seq_along(facet_splits), function(i) {
    facet_data <- facet_splits[[i]]
    result <- fix_facet_variance_grouped(facet_data, x, y, group_by)

    # Restore facet_by columns if they exist
    if (!is.null(facet_by)) {
      facet_values <- unique(facet_data[, facet_by, drop = FALSE])
      if (nrow(facet_values) == 1) {
        for (fb in facet_by) {
          result[[fb]] <- facet_values[[fb]]
        }
      }
    }

    return(result)
  })

  do.call(rbind, fixed_data_list)
}

#' Preprocess data for pairwise comparisons without group_by
#'
#' @description
#' Prepares data for simple (non-grouped) pairwise statistical comparisons
#' by detecting and fixing variance issues.
#'
#' @param data Data frame containing the plot data
#' @param x Character string; x-axis column name
#' @param y Character string; y-axis column name
#' @param facet_by Character vector; faceting column name(s), or NULL
#'
#' @return Preprocessed data frame ready for statistical comparisons
#'
#' @examples
#' \dontrun{
#' # Sample data
#' df <- data.frame(
#'   category = factor(rep(LETTERS[1:3], each = 10)),
#'   value = c(rnorm(10, 5), rnorm(10, 7), rnorm(10, 6))
#' )
#'
#' # Preprocess for simple comparisons
#' df_processed <- preprocess_comparison_data_simple(df, "category", "value")
#' }
#'
#' @keywords internal
preprocess_comparison_data_simple <- function(data, x, y, facet_by = NULL) {
  split_cols <- c(x, y)
  grouping_vars <- x

  if (!is.null(facet_by)) {
    split_cols <- c(split_cols, facet_by)
    grouping_vars <- c(grouping_vars, facet_by)
  }

  # Create grouping key
  if (length(grouping_vars) > 1) {
    split_key <- interaction(data[grouping_vars], drop = TRUE, sep = " // ")
  } else {
    split_key <- data[[grouping_vars]]
  }

  data_groups <- split(data[, split_cols, drop = FALSE], split_key)
  needs_fix <- check_variance_issues_simple(data_groups, y)

  if (!needs_fix) {
    return(data)
  }

  warning("Some pairwise comparisons may fail due to insufficient variability. Adjusting data to ensure valid comparisons.")

  # Split by facet if present
  if (!is.null(facet_by)) {
    facet_key <- interaction(data[facet_by], drop = TRUE, sep = " // ")
    facet_splits <- split(data[, split_cols, drop = FALSE], facet_key)
  } else {
    facet_splits <- list(data[, split_cols, drop = FALSE])
  }

  fixed_data_list <- lapply(seq_along(facet_splits), function(i) {
    facet_data <- facet_splits[[i]]
    result <- fix_facet_variance_simple(facet_data, x, y)

    # Restore facet_by columns if they exist
    if (!is.null(facet_by)) {
      facet_values <- unique(facet_data[, facet_by, drop = FALSE])
      if (nrow(facet_values) == 1) {
        for (fb in facet_by) {
          result[[fb]] <- facet_values[[fb]]
        }
      }
    }

    return(result)
  })

  do.call(rbind, fixed_data_list)
}

# ============================================================================
# Helper Functions for Paired Data Validation
# ============================================================================

#' Format paired validation error message
#'
#' @description
#' Generates a human-readable error message when paired data validation fails.
#'
#' @param problem_groups Data frame containing problematic group combinations
#' @param x Character string; x-axis column name
#' @param paired_by Character string; pairing column name
#' @param group_by Character string; grouping column name (can be NULL)
#' @param n_total_col Character string; name of the count column
#' @param with_group Logical; whether group_by is used
#'
#' @return Character string containing the formatted error message
#'
#' @keywords internal
format_paired_error <- function(problem_groups, x, paired_by, group_by = NULL,
                                n_total_col, with_group = TRUE) {
  cols_to_show <- if (with_group) {
    c(x, paired_by, group_by, ".n", n_total_col)
  } else {
    c(x, paired_by, ".n", n_total_col)
  }

  error_details <- apply(problem_groups[, cols_to_show], 1, function(row) {
    paste(paste(names(row), row, sep = "="), collapse = ", ")
  })

  base_msg <- if (with_group) {
    "When 'paired_by' and 'group_by' are both provided, each combination of 'x' and 'paired_by' must have exactly two observations, one for each group in 'group_by'."
  } else {
    "When 'paired_by' is provided without 'group_by', each combination of 'x' and 'paired_by' must have exactly two observations, one for each value of 'x'."
  }

  paste0(
    base_msg,
    " The following combinations do not satisfy this requirement:\n",
    paste0(error_details, collapse = "\n")
  )
}

#' Validate paired data with groups
#'
#' @description
#' Validates that paired data with grouping has the correct structure:
#' each combination of x and paired_by should have exactly two observations,
#' one for each group.
#'
#' @param data Data frame to validate
#' @param x Character string; x-axis column name
#' @param paired_by Character string; pairing column name
#' @param group_by Character string; grouping column name
#'
#' @return NULL (invisibly); throws error if validation fails
#'
#' @examples
#' \dontrun{
#' # Valid paired data with groups
#' df <- data.frame(
#'   time = rep(c("Before", "After"), each = 6),
#'   subject = rep(1:6, 2),
#'   group = rep(c("Control", "Treatment"), 6),
#'   value = rnorm(12)
#' )
#' validate_paired_groups(df, "time", "subject", "group")
#'
#' # Invalid data will throw an error
#' df_invalid <- df[-1, ] # Remove one observation
#' # validate_paired_groups(df_invalid, "time", "subject", "group")
#' # Error: validation fails
#' }
#'
#' @keywords internal
#' @importFrom dplyr group_by summarise add_count filter mutate
#' @importFrom rlang syms sym
validate_paired_groups <- function(data, x, paired_by, group_by) {
  n_total_col <- paste0(".n_total_", paired_by)

  problem_groups <- data %>%
    dplyr::group_by(!!!syms(c(x, paired_by, group_by))) %>%
    dplyr::summarise(.n = dplyr::n(), .groups = "drop") %>%
    dplyr::add_count(!!!syms(c(x, paired_by)), name = n_total_col) %>%
    dplyr::filter(!!sym(".n") != 1 | !!sym(n_total_col) != 2) %>%
    dplyr::mutate(
      .n = ifelse(!!sym(".n") == 1, !!sym(".n"), paste0(!!sym(".n"), " (expecting 1)")),
      !!sym(n_total_col) := ifelse(!!sym(n_total_col) == 2, !!sym(n_total_col),
        paste0(!!sym(n_total_col), " (expecting 2)")
      )
    )

  if (nrow(problem_groups) > 0) {
    stop(format_paired_error(problem_groups, x, paired_by, group_by, n_total_col, TRUE))
  }
}

#' Validate paired data without groups
#'
#' @description
#' Validates that paired data without grouping has the correct structure:
#' exactly two unique x values, and each paired_by value appears exactly
#' once for each x value.
#'
#' @param data Data frame to validate
#' @param x Character string; x-axis column name
#' @param paired_by Character string; pairing column name
#'
#' @return NULL (invisibly); throws error if validation fails
#'
#' @examples
#' \dontrun{
#' # Valid paired data
#' df <- data.frame(
#'   time = rep(c("Pre", "Post"), each = 5),
#'   subject = rep(1:5, 2),
#'   value = rnorm(10)
#' )
#' validate_paired_simple(df, "time", "subject")
#'
#' # Invalid: more than 2 x values
#' df_invalid <- data.frame(
#'   time = rep(c("T1", "T2", "T3"), each = 5),
#'   subject = rep(1:5, 3),
#'   value = rnorm(15)
#' )
#' # validate_paired_simple(df_invalid, "time", "subject")
#' # Error: Exactly two unique values of 'x' are required
#' }
#'
#' @keywords internal
#' @importFrom dplyr group_by summarise add_count filter mutate n_distinct
#' @importFrom rlang syms sym
validate_paired_simple <- function(data, x, paired_by) {
  if (dplyr::n_distinct(data[[x]], na.rm = TRUE) != 2) {
    stop("Exactly two unique values of 'x' are required when 'paired_by' is provided without 'group_by'.")
  }

  n_total_col <- paste0(".n_total_", paired_by)

  problem_groups <- data %>%
    dplyr::group_by(!!!syms(c(x, paired_by))) %>%
    dplyr::summarise(.n = dplyr::n(), .groups = "drop") %>%
    dplyr::add_count(!!!syms(paired_by), name = n_total_col) %>%
    dplyr::filter(!!sym(".n") != 1 | !!sym(n_total_col) != 2) %>%
    dplyr::mutate(
      .n = ifelse(!!sym(".n") == 1, !!sym(".n"), paste0(!!sym(".n"), " (expecting 1)")),
      !!sym(n_total_col) := ifelse(!!sym(n_total_col) == 2, !!sym(n_total_col),
        paste0(!!sym(n_total_col), " (expecting 2)")
      )
    )

  if (nrow(problem_groups) > 0) {
    stop(format_paired_error(problem_groups, x, paired_by, NULL, n_total_col, FALSE))
  }
}

#' Validate paired data structure
#'
#' @description
#' Main validation function for paired data. Handles NA values in paired_by,
#' validates structure based on whether group_by is provided, and sorts
#' data for proper pairing.
#'
#' @param data Data frame to validate
#' @param x Character string; x-axis column name
#' @param y Character string; y-axis column name
#' @param paired_by Character string; pairing column name
#' @param group_by Character string; grouping column name (can be NULL)
#'
#' @return Validated and sorted data frame
#'
#' @examples
#' \dontrun{
#' # Paired t-test scenario: before/after measurements
#' df <- data.frame(
#'   timepoint = factor(rep(c("Before", "After"), each = 10)),
#'   subject_id = factor(rep(1:10, 2)),
#'   measurement = c(rnorm(10, 100, 15), rnorm(10, 95, 15))
#' )
#'
#' # Validate and sort data
#' df_validated <- validate_paired_data(df, "timepoint", "measurement", "subject_id")
#'
#' # With group_by (e.g., treatment groups)
#' df_grouped <- data.frame(
#'   timepoint = rep(c("Before", "After"), 20),
#'   subject_id = factor(rep(1:20, each = 2)),
#'   treatment = factor(rep(c("Placebo", "Drug"), each = 20)),
#'   measurement = rnorm(40, 100, 15)
#' )
#' df_validated2 <- validate_paired_data(
#'   df_grouped, "timepoint", "measurement", "subject_id", "treatment"
#' )
#' }
#'
#' @keywords internal
#' @importFrom dplyr arrange
#' @importFrom rlang syms
validate_paired_data <- function(data, x, y, paired_by, group_by = NULL) {
  if (any(is.na(data[[paired_by]]))) {
    warning("'paired_by' contains missing values, removing corresponding rows.")
    data <- data[!is.na(data[[paired_by]]), , drop = FALSE]
  }

  if (!is.null(group_by)) {
    validate_paired_groups(data, x, paired_by, group_by)
  } else {
    validate_paired_simple(data, x, paired_by)
  }

  # Sort by paired_by to ensure correspondence
  data %>% dplyr::arrange(!!!syms(unique(c(paired_by, x, group_by))))
}

# ============================================================================
# Helper Functions for Plot Dimensions
# ============================================================================

#' Check if facets have free scales
#'
#' @description
#' Determines whether faceted plot has free scales on the relevant axis
#' (y-axis for non-flipped, x-axis for flipped plots).
#'
#' @param facet_by Character vector; faceting column(s), or NULL
#' @param facet_scales Character string; facet scales setting
#' @param flip Logical; whether coordinates are flipped
#'
#' @return Logical; TRUE if facets have free scales on relevant axis
#'
#' @examples
#' \dontrun{
#' # Free y-axis (non-flipped plot)
#' is_facet_free("panel", "free_y", flip = FALSE)
#' # [1] TRUE
#'
#' # Free x-axis (flipped plot)
#' is_facet_free("panel", "free_x", flip = TRUE)
#' # [1] TRUE
#'
#' # No faceting
#' is_facet_free(NULL, "free", flip = FALSE)
#' # [1] FALSE
#' }
#'
#' @keywords internal
is_facet_free <- function(facet_by, facet_scales, flip) {
  !is.null(facet_by) && (
    identical(facet_scales, "free") ||
      (!flip && identical(facet_scales, "free_y")) ||
      (flip && identical(facet_scales, "free_x"))
  )
}

#' Calculate base plot dimensions
#'
#' @description
#' Calculates optimal plot dimensions based on plot orientation,
#' stacking, label length, and number of categories/groups.
#'
#' @param flip Logical; whether coordinates are flipped
#' @param stack Logical; whether facets are stacked
#' @param x_maxchars Integer; maximum character length of x-axis labels
#' @param nx Integer; number of x-axis categories
#' @param nd Integer; number of groups (1 if no grouping)
#'
#' @return List with width, height, facet_nrow, facet_ncol, strip_position
#'
#' @examples
#' \dontrun{
#' # Standard vertical boxplot
#' calculate_base_dimensions(
#'   flip = FALSE, stack = FALSE,
#'   x_maxchars = 10, nx = 5, nd = 2
#' )
#'
#' # Horizontal boxplot with stacked facets
#' calculate_base_dimensions(
#'   flip = TRUE, stack = TRUE,
#'   x_maxchars = 15, nx = 4, nd = 1
#' )
#' }
#'
#' @keywords internal
calculate_base_dimensions <- function(flip, stack, x_maxchars, nx, nd) {
  if (isTRUE(flip) && isTRUE(stack)) {
    list(
      width = max(3, 2 + x_maxchars * 0.05),
      height = nx * nd * 0.3,
      facet_nrow = 1,
      facet_ncol = NULL,
      strip_position = "top"
    )
  } else if (isTRUE(flip) && isFALSE(stack)) {
    list(
      width = max(3, 2.2 + x_maxchars * 0.05),
      height = nx * nd * 0.3,
      facet_nrow = NULL,
      facet_ncol = NULL,
      strip_position = "top"
    )
  } else if (isTRUE(stack)) {
    list(
      width = nx * nd * 0.3,
      height = 4 + x_maxchars * 0.05,
      facet_nrow = NULL,
      facet_ncol = 1,
      strip_position = "right"
    )
  } else {
    list(
      width = nx * nd * 0.3,
      height = max(3, 2 + x_maxchars * 0.05),
      facet_nrow = NULL,
      facet_ncol = NULL,
      strip_position = "top"
    )
  }
}

#' Calculate legend dimensions
#'
#' @description
#' Calculates additional space needed for the legend based on its
#' position and direction.
#'
#' @param legend.position Character string; legend position
#' @param legend.direction Character string; legend direction
#'
#' @return List with width and height additions
#'
#' @keywords internal
calculate_legend_dimensions <- function(legend.position, legend.direction) {
  if (identical(legend.position, "none")) {
    return(list(width = 0, height = 0))
  }

  if (legend.position %in% c("right", "left")) {
    list(width = 1, height = 0)
  } else if (legend.direction == "horizontal") {
    list(width = 0, height = 1)
  } else {
    list(width = 2, height = 0)
  }
}

#' Calculate plot dimensions and layout
#'
#' @description
#' Main function for calculating optimal plot dimensions and layout settings.
#' Combines base dimensions, legend space, and coordinate system settings.
#'
#' @param data Data frame containing the plot data
#' @param x Character string; x-axis column name
#' @param group_by Character string; grouping column name (can be NULL)
#' @param flip Logical; whether coordinates are flipped
#' @param stack Logical; whether facets are stacked
#' @param facet_by Character vector; faceting column(s)
#' @param facet_scales Character string; facet scales setting
#' @param facet_nrow Integer; number of facet rows (can be NULL)
#' @param facet_ncol Integer; number of facet columns (can be NULL)
#' @param legend.position Character string; legend position
#' @param legend.direction Character string; legend direction
#' @param y_min_use Numeric; y-axis minimum value
#' @param y_max_use Numeric; y-axis maximum value
#'
#' @return List with width, height, facet_nrow, facet_ncol, strip_position, coord
#'
#' @keywords internal
#' @importFrom ggplot2 coord_flip coord_cartesian
calculate_plot_layout <- function(data, x, group_by, flip, stack, facet_by, facet_scales,
                                  facet_nrow, facet_ncol, legend.position, legend.direction,
                                  y_min_use, y_max_use) {
  x_maxchars <- max(nchar(levels(data[[x]])))
  nx <- nlevels(data[[x]])
  nd <- ifelse(is.null(group_by), 1, nlevels(data[[group_by]]))
  facet_free <- is_facet_free(facet_by, facet_scales, flip)

  base_dims <- calculate_base_dimensions(flip, stack, x_maxchars, nx, nd)
  legend_dims <- calculate_legend_dimensions(legend.position, legend.direction)

  # Override facet dimensions if provided
  if (!is.null(facet_nrow)) base_dims$facet_nrow <- facet_nrow
  if (!is.null(facet_ncol)) base_dims$facet_ncol <- facet_ncol

  list(
    width = base_dims$width + legend_dims$width,
    height = max(base_dims$height + legend_dims$height, base_dims$width + legend_dims$width),
    facet_nrow = base_dims$facet_nrow,
    facet_ncol = base_dims$facet_ncol,
    strip_position = base_dims$strip_position,
    coord = if (isTRUE(flip)) {
      if (facet_free) coord_flip() else coord_flip(ylim = c(y_min_use, y_max_use))
    } else {
      if (facet_free) NULL else coord_cartesian(ylim = c(y_min_use, y_max_use))
    }
  )
}

# ============================================================================
# Helper Functions for Plot Layers
# ============================================================================

#' Apply plot theme with consistent defaults
#'
#' @description
#' Applies a consistent theme to box/violin plots with optimized settings
#' for readability and aesthetics. Handles font sizing, grid lines, spacing,
#' and axis text orientation.
#'
#' @param p ggplot object to modify
#' @param theme Theme function to use
#' @param theme_args List of additional theme arguments
#' @param layout_info List containing layout information
#' @param aspect.ratio Numeric; plot aspect ratio
#' @param legend.position Character string; legend position
#' @param legend.direction Character string; legend direction
#' @param x_text_angle Numeric; x-axis text angle (0, 45, or 90)
#' @param just List with h and v justification values
#' @param flip Logical; whether coordinates are flipped
#' @param stack Logical; whether facets are stacked
#' @param data Data frame containing the plot data
#' @param x Character string; x-axis column name
#' @param y Character string; y-axis column name
#'
#' @return Modified ggplot object with theme applied
#'
#' @keywords internal
#' @importFrom ggplot2 element_line element_text unit
apply_boxviolin_theme <- function(p, theme, theme_args, layout_info, aspect.ratio,
                                  legend.position, legend.direction, x_text_angle, just,
                                  flip, stack, data, x, y) {
  # Grid direction
  grid_axis <- if (isTRUE(flip)) "panel.grid.major.x" else "panel.grid.major.y"

  # Default theme settings with optimized font sizes
  default_theme_args <- list(
    panel.grid.major = element_line(colour = "grey80", linetype = 2),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(face = "bold", size = 12, color = "black"),
    axis.title.y = element_text(face = "bold", size = 12, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13, color = "black")
  )

  # Stack-specific theme adjustments
  if (isTRUE(flip) && isTRUE(stack)) {
    default_theme_args$panel.spacing.x <- unit(-1, "pt")
    default_theme_args$strip.text.x <- element_text(angle = 0)
  } else if (isTRUE(flip) && isFALSE(stack)) {
    default_theme_args$strip.text.y <- element_text(angle = 0)
  } else if (isTRUE(stack)) {
    default_theme_args$panel.spacing.y <- unit(-1, "pt")
    default_theme_args$strip.text.y <- element_text(angle = 0, hjust = 0)
  } else {
    default_theme_args$strip.text.x <- element_text(angle = 0)
  }

  # Add grid line for appropriate axis
  default_theme_args[[grid_axis]] <- ggplot2::element_line(color = "grey", linetype = 2)

  # Apply theme with user overrides
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(
      aspect.ratio = aspect.ratio,
      panel.grid.major = ggplot2::element_line(colour = "grey80", linetype = 2)
    )

  # Apply data-driven styling (with x-axis angle override)
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = if (flip) y else x,
    y_var = if (flip) x else y,
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction,
    axis.text.x = ggplot2::element_text(
      angle = x_text_angle,
      hjust = just$h,
      vjust = just$v
    )
  )

  return(p)
}

# ============================================================================
# Wrapper Functions
# ============================================================================

#' Box/Violin Plot
#'
#' @description
#' Create box or violin plots with optional grouping, faceting, and statistical
#' comparisons. This is the main wrapper function that handles data splitting
#' and combines multiple plots.
#'
#' @inheritParams parameters
#' @param x Column for x-axis (discrete). Can be a single column name or
#'   multiple columns that will be concatenated.
#' @param y Column for y-axis (numeric). The response variable.
#' @param base Plot base type: "box" or "violin"
#' @param in_form Input data form: "long" (default) or "wide"
#' @param sort_x Sort x-axis values: "none", "mean_asc", "mean_desc", "mean",
#'   "median_asc", "median_desc", "median"
#' @param flip Logical; flip coordinates to create horizontal plots
#' @param keep_empty Logical; keep empty factor levels on x-axis
#' @param group_by Column for grouping (creates dodged/side-by-side plots)
#' @param group_name Legend name for groups
#' @param paired_by Column identifying paired observations (for paired tests)
#' @param fill_mode Fill coloring mode: "dodge", "x", "mean", or "median"
#' @param fill_reverse Logical; reverse gradient fills
#' @param add_point Logical; add jittered data points
#' @param pt_color Point color (default: "grey30")
#' @param pt_size Point size (auto-calculated if NULL)
#' @param pt_alpha Point transparency (0-1)
#' @param jitter_width Jitter width for points
#' @param jitter_height Jitter height for points
#' @param stack Logical; stack facets vertically/horizontally
#' @param y_max Y-axis maximum (numeric or "qXX" for quantile)
#' @param y_min Y-axis minimum (numeric or "qXX" for quantile)
#' @param y_trans Y-axis transformation (e.g., "log10", "sqrt")
#' @param y_nbreaks Number of y-axis breaks
#' @param step_increase Step increase for comparison brackets
#' @param symnum_args Symbolic number coding arguments for significance
#' @param add_box Logical; add box overlay (violin only)
#' @param box_color Box overlay color
#' @param box_width Box overlay width
#' @param box_ptsize Box median point size
#' @param add_trend Logical; add trend line connecting medians
#' @param trend_color Trend line color
#' @param trend_linewidth Trend line width
#' @param trend_ptsize Trend point size
#' @param add_stat Function to add stat summary (e.g., mean)
#' @param stat_name Stat legend name
#' @param stat_color Stat point color
#' @param stat_size Stat point size
#' @param stat_stroke Stat point stroke width
#' @param stat_shape Stat point shape
#' @param add_bg Logical; add alternating background
#' @param bg_palette Background color palette
#' @param bg_palcolor Background custom colors
#' @param bg_alpha Background transparency
#' @param add_line Numeric; add horizontal reference line at this value
#' @param line_color Reference line color
#' @param line_width Reference line width
#' @param line_type Reference line type
#' @param highlight Points to highlight (logical, indices, or expression)
#' @param highlight_color Highlight color
#' @param highlight_size Highlight size
#' @param highlight_alpha Highlight transparency
#' @param comparisons Pairwise comparisons (list of pairs or TRUE for all)
#' @param ref_group Reference group for comparisons
#' @param pairwise_method Statistical method for pairwise comparisons
#' @param multiplegroup_comparisons Logical; perform multiple group comparisons
#' @param multiple_method Statistical method for multiple comparisons
#' @param sig_label Significance label format: "p.format" or "p.signif"
#' @param sig_labelsize Significance label font size
#' @param hide_ns Logical; hide non-significant comparisons
#'
#' @return A ggplot object or combined plots (patchwork)
#'
#' @keywords internal
#' @importFrom rlang "%||%"
BoxViolinPlot <- function(
    data, x, x_sep = "_", y = NULL, base = c("box", "violin"), in_form = c("long", "wide"),
    split_by = NULL, split_by_sep = "_", symnum_args = NULL,
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    paired_by = NULL, x_text_angle = NULL, step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_ggforge", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = "grey30", pt_size = NULL, pt_alpha = 1,
    jitter_width = NULL, jitter_height = 0, stack = FALSE, y_max = NULL, y_min = NULL,
    add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = "p.format", sig_labelsize = 3.5, hide_ns = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...) {
  validate_common_args(seed)
  theme <- process_theme(theme)
  split_by <- validate_columns(data, split_by,
    force_factor = TRUE, allow_multi = TRUE,
    concat_multi = TRUE, concat_sep = split_by_sep
  )

  if (!is.null(split_by)) {
    datas <- split(data, data[[split_by]])
    datas <- datas[levels(data[[split_by]])]
  } else {
    datas <- list(data)
    names(datas) <- "..."
  }

  palette <- check_palette(palette, names(datas))
  palcolor <- check_palcolor(palcolor, names(datas))
  legend.direction <- check_legend_param(legend.direction, names(datas), "legend.direction")
  legend.position <- check_legend_param(legend.position, names(datas), "legend.position")

  stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")

  plots <- lapply(
    names(datas), function(nm) {
      default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
      if (is.function(title)) {
        title <- title(default_title)
      } else {
        title <- title %||% default_title
      }

      BoxViolinPlotAtomic(datas[[nm]],
        x = x, x_sep = x_sep, y = y, base = base, in_form = in_form,
        sort_x = sort_x, flip = flip, keep_empty = keep_empty,
        group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
        paired_by = paired_by, x_text_angle = x_text_angle,
        fill_mode = fill_mode, fill_reverse = fill_reverse, step_increase = step_increase,
        theme = theme, theme_args = theme_args, palette = palette[[nm]],
        palcolor = palcolor[[nm]], alpha = alpha,
        aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]],
        legend.direction = legend.direction[[nm]],
        add_point = add_point, pt_color = pt_color, pt_size = pt_size,
        pt_alpha = pt_alpha, symnum_args = symnum_args,
        jitter_width = jitter_width, jitter_height = jitter_height,
        stack = stack, y_max = y_max, y_min = y_min,
        add_box = add_box, box_color = box_color, box_width = box_width,
        box_ptsize = box_ptsize,
        add_trend = add_trend, trend_color = trend_color,
        trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
        add_stat = add_stat, stat_name = stat_name, stat_color = stat_color,
        stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
        add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor,
        bg_alpha = bg_alpha,
        add_line = add_line, line_color = line_color, line_width = line_width,
        line_type = line_type,
        highlight = highlight, highlight_color = highlight_color,
        highlight_size = highlight_size, highlight_alpha = highlight_alpha,
        comparisons = comparisons, ref_group = ref_group,
        pairwise_method = pairwise_method,
        multiplegroup_comparisons = multiplegroup_comparisons,
        multiple_method = multiple_method,
        sig_label = sig_label, sig_labelsize = sig_labelsize, hide_ns = hide_ns,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol,
        facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
        seed = seed, ...
      )
    }
  )

  combine_plots(plots,
    combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
    axes = axes, axis_titles = axis_titles, guides = guides, design = design
  )
}

#' Box Plot
#'
#' @description
#' Create box plots with optional grouping, faceting, and statistical comparisons.
#' Box plots display the distribution of continuous data through their quartiles,
#' showing the median, interquartile range, and potential outliers.
#'
#' @inheritParams BoxViolinPlot
#'
#' @return A ggplot object or combined plots (patchwork)
#'
#' @export
#'
#' @examples
#' \donttest{
#' # ============================================================
#' # Basic Examples
#' # ============================================================
#'
#' # Create sample data
#' set.seed(123)
#' data <- data.frame(
#'   group = rep(LETTERS[1:4], each = 25),
#'   value = c(
#'     rnorm(25, 10, 2), rnorm(25, 12, 2),
#'     rnorm(25, 11, 3), rnorm(25, 14, 2)
#'   ),
#'   treatment = rep(c("Control", "Treatment"), 50),
#'   gender = sample(c("Male", "Female"), 100, replace = TRUE)
#' )
#'
#' # Simple box plot
#' BoxPlot(data, x = "group", y = "value")
#'
#' # Box plot with custom labels
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   title = "Distribution by Group",
#'   xlab = "Experimental Group",
#'   ylab = "Measurement Value"
#' )
#'
#' # ============================================================
#' # Grouped Box Plots
#' # ============================================================
#'
#' # Side-by-side (dodged) box plots by treatment
#' BoxPlot(data, x = "group", y = "value", group_by = "treatment")
#'
#' # With custom group legend name
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   group_by = "treatment",
#'   group_name = "Treatment Group"
#' )
#'
#' # With custom color palette
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   group_by = "treatment",
#'   palette = "Set1"
#' )
#'
#' # ============================================================
#' # Adding Data Points
#' # ============================================================
#'
#' # Add jittered points to show individual observations
#' BoxPlot(data, x = "group", y = "value", add_point = TRUE)
#'
#' # Customize point appearance
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   add_point = TRUE,
#'   pt_color = "darkblue",
#'   pt_size = 1.5,
#'   pt_alpha = 0.6
#' )
#'
#' # With grouped data
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   group_by = "treatment",
#'   add_point = TRUE,
#'   pt_alpha = 0.5
#' )
#'
#' # ============================================================
#' # Highlighting Specific Points
#' # ============================================================
#'
#' # Highlight outliers or specific observations
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   add_point = TRUE,
#'   highlight = "value > 15",
#'   highlight_color = "red",
#'   highlight_size = 3
#' )
#'
#' # Highlight by row indices
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   add_point = TRUE,
#'   highlight = c(1, 5, 10, 15),
#'   highlight_color = "orange"
#' )
#'
#' # ============================================================
#' # Statistical Comparisons
#' # ============================================================
#'
#' # Compare specific pairs
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   comparisons = list(c("A", "B"), c("A", "D"))
#' )
#'
#' # Compare all pairs
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   comparisons = TRUE
#' )
#'
#' # Use t-test instead of Wilcoxon
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   comparisons = list(c("A", "D")),
#'   pairwise_method = "t.test"
#' )
#'
#' # Show significance symbols instead of p-values
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   comparisons = TRUE,
#'   sig_label = "p.signif"
#' )
#'
#' # Hide non-significant comparisons
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   comparisons = TRUE,
#'   hide_ns = TRUE
#' )
#'
#' # With grouped data - compare groups within each x category
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   group_by = "treatment",
#'   comparisons = TRUE
#' )
#'
#' # Multiple group comparison (Kruskal-Wallis)
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   multiplegroup_comparisons = TRUE
#' )
#'
#' # ============================================================
#' # Paired Data Analysis
#' # ============================================================
#'
#' # Create paired data (before/after measurements)
#' paired_data <- data.frame(
#'   time = factor(rep(c("Before", "After"), each = 20)),
#'   subject = factor(rep(1:20, 2)),
#'   score = c(rnorm(20, 50, 10), rnorm(20, 55, 10))
#' )
#'
#' # Paired box plot with connecting lines
#' BoxPlot(paired_data,
#'   x = "time", y = "score",
#'   paired_by = "subject",
#'   add_point = TRUE
#' )
#'
#' # With paired statistical test
#' BoxPlot(paired_data,
#'   x = "time", y = "score",
#'   paired_by = "subject",
#'   comparisons = list(c("Before", "After")),
#'   pairwise_method = "t.test"
#' )
#'
#' # ============================================================
#' # Sorting and Orientation
#' # ============================================================
#'
#' # Sort by mean value (ascending)
#' BoxPlot(data, x = "group", y = "value", sort_x = "mean_asc")
#'
#' # Sort by median value (descending)
#' BoxPlot(data, x = "group", y = "value", sort_x = "median_desc")
#'
#' # Horizontal box plot
#' BoxPlot(data, x = "group", y = "value", flip = TRUE)
#'
#' # ============================================================
#' # Axis Customization
#' # ============================================================
#'
#' # Set y-axis limits
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   y_min = 5, y_max = 20
#' )
#'
#' # Use quantiles for y-axis limits (exclude extreme values)
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   y_min = "q5", y_max = "q95"
#' )
#'
#' # Log-transform y-axis
#' pos_data <- data
#' pos_data$value <- abs(pos_data$value) + 1
#' BoxPlot(pos_data,
#'   x = "group", y = "value",
#'   y_trans = "log10"
#' )
#'
#' # ============================================================
#' # Visual Enhancements
#' # ============================================================
#'
#' # Add trend line connecting medians
#' BoxPlot(data, x = "group", y = "value", add_trend = TRUE)
#'
#' # Add reference line
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   add_line = 12,
#'   line_color = "red",
#'   line_type = 2
#' )
#'
#' # Add alternating background
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   add_bg = TRUE,
#'   bg_alpha = 0.1
#' )
#'
#' # Add mean indicator
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   add_stat = mean,
#'   stat_name = "Mean",
#'   stat_color = "red",
#'   stat_shape = 18
#' )
#'
#' # ============================================================
#' # Faceting
#' # ============================================================
#'
#' # Facet by another variable
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   facet_by = "gender"
#' )
#'
#' # Control facet layout
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   facet_by = "gender",
#'   facet_ncol = 2
#' )
#'
#' # Free y-axis scales per facet
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   facet_by = "gender",
#'   facet_scales = "free_y"
#' )
#'
#' # ============================================================
#' # Fill Modes
#' # ============================================================
#'
#' # Fill by x-axis category (default when no group_by)
#' BoxPlot(data, x = "group", y = "value", fill_mode = "x")
#'
#' # Fill by mean value (gradient)
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   fill_mode = "mean",
#'   palette = "RdYlBu"
#' )
#'
#' # Fill by median value (gradient)
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   fill_mode = "median",
#'   palette = "viridis"
#' )
#'
#' # ============================================================
#' # Wide Format Data
#' # ============================================================
#'
#' # Wide format: each column is a group
#' wide_data <- data.frame(
#'   GroupA = rnorm(30, 10, 2),
#'   GroupB = rnorm(30, 12, 2),
#'   GroupC = rnorm(30, 11, 3)
#' )
#'
#' BoxPlot(wide_data,
#'   x = c("GroupA", "GroupB", "GroupC"),
#'   in_form = "wide"
#' )
#'
#' # ============================================================
#' # Splitting into Multiple Plots
#' # ============================================================
#'
#' # Create separate plots by a variable
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   split_by = "gender",
#'   combine = TRUE,
#'   ncol = 2
#' )
#'
#' # ============================================================
#' # Theme and Style Customization
#' # ============================================================
#'
#' # Custom color palette
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   palette = "Dark2"
#' )
#'
#' # Custom colors
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   palcolor = c(
#'     "A" = "#E41A1C", "B" = "#377EB8",
#'     "C" = "#4DAF4A", "D" = "#984EA3"
#'   )
#' )
#'
#' # Legend position
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   group_by = "treatment",
#'   legend.position = "bottom",
#'   legend.direction = "horizontal"
#' )
#'
#' # Hide legend
#' BoxPlot(data,
#'   x = "group", y = "value",
#'   group_by = "treatment",
#'   legend.position = "none"
#' )
#' }
BoxPlot <- function(
    data, x, x_sep = "_", y = NULL, in_form = c("long", "wide"),
    split_by = NULL, split_by_sep = "_", symnum_args = NULL,
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    paired_by = NULL, x_text_angle = NULL, step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_ggforge", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = "grey30", pt_size = NULL, pt_alpha = 1,
    jitter_width = NULL, jitter_height = 0, stack = FALSE, y_max = NULL, y_min = NULL,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = "p.format", sig_labelsize = 3.5, hide_ns = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, ...) {
  stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")

  BoxViolinPlot(
    data = data, x = x, x_sep = x_sep, y = y, base = "box", in_form = in_form,
    split_by = split_by, split_by_sep = split_by_sep,
    sort_x = sort_x, flip = flip, keep_empty = keep_empty,
    group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
    paired_by = paired_by, x_text_angle = x_text_angle,
    fill_mode = fill_mode, fill_reverse = fill_reverse, step_increase = step_increase,
    theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
    aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
    add_point = add_point, pt_color = pt_color, pt_size = pt_size, pt_alpha = pt_alpha,
    symnum_args = symnum_args,
    jitter_width = jitter_width, jitter_height = jitter_height, stack = stack,
    y_max = y_max, y_min = y_min,
    add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth,
    trend_ptsize = trend_ptsize,
    add_stat = add_stat, stat_name = stat_name, stat_color = stat_color,
    stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
    add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
    add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
    highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size,
    highlight_alpha = highlight_alpha,
    comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
    multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
    sig_label = sig_label, sig_labelsize = sig_labelsize, hide_ns = hide_ns,
    facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol,
    facet_nrow = facet_nrow, facet_byrow = facet_byrow,
    title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed,
    combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
    axes = axes, axis_titles = axis_titles, guides = guides, ...
  )
}

#' Violin Plot
#'
#' @description
#' Create violin plots with optional grouping, faceting, and statistical comparisons.
#' Violin plots combine box plots with kernel density estimation to show the
#' distribution shape of continuous data.
#'
#' @inheritParams BoxViolinPlot
#'
#' @return A ggplot object or combined plots (patchwork)
#'
#' @export
#'
#' @examples
#' \donttest{
#' # ============================================================
#' # Basic Examples
#' # ============================================================
#'
#' # Create sample data with different distributions
#' set.seed(456)
#' data <- data.frame(
#'   category = rep(c("Normal", "Bimodal", "Skewed", "Uniform"), each = 100),
#'   value = c(
#'     rnorm(100, 50, 10), # Normal
#'     c(rnorm(50, 30, 5), rnorm(50, 70, 5)), # Bimodal
#'     rexp(100, 0.1), # Skewed
#'     runif(100, 20, 80) # Uniform
#'   ),
#'   group = rep(c("A", "B"), 200)
#' )
#'
#' # Simple violin plot
#' ViolinPlot(data, x = "category", y = "value")
#'
#' # With title and labels
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   title = "Distribution Shapes Comparison",
#'   xlab = "Distribution Type",
#'   ylab = "Value"
#' )
#'
#' # ============================================================
#' # Violin with Box Plot Overlay
#' # ============================================================
#'
#' # Add box plot inside violin (shows quartiles)
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   add_box = TRUE
#' )
#'
#' # Customize box overlay
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   add_box = TRUE,
#'   box_color = "white",
#'   box_width = 0.15,
#'   box_ptsize = 3
#' )
#'
#' # ============================================================
#' # Adding Data Points
#' # ============================================================
#'
#' # Add jittered points to show individual observations
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   add_point = TRUE
#' )
#'
#' # Combine box overlay with points
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   add_box = TRUE,
#'   add_point = TRUE,
#'   pt_alpha = 0.3
#' )
#'
#' # Customize point appearance
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   add_point = TRUE,
#'   pt_color = "navy",
#'   pt_size = 0.8,
#'   pt_alpha = 0.5,
#'   jitter_width = 0.3
#' )
#'
#' # ============================================================
#' # Grouped Violin Plots
#' # ============================================================
#'
#' # Side-by-side violins by group
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   group_by = "group"
#' )
#'
#' # With box overlay
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   group_by = "group",
#'   add_box = TRUE
#' )
#'
#' # Custom palette
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   group_by = "group",
#'   palette = "Set2",
#'   add_box = TRUE
#' )
#'
#' # ============================================================
#' # Statistical Comparisons
#' # ============================================================
#'
#' # Compare specific distributions
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   comparisons = list(
#'     c("Normal", "Bimodal"),
#'     c("Normal", "Skewed")
#'   )
#' )
#'
#' # All pairwise comparisons
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   comparisons = TRUE,
#'   sig_label = "p.signif"
#' )
#'
#' # Grouped comparisons
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   group_by = "group",
#'   comparisons = TRUE
#' )
#'
#' # ============================================================
#' # Paired Data Analysis
#' # ============================================================
#'
#' # Create paired data
#' paired_data <- data.frame(
#'   condition = factor(rep(c("Baseline", "Treatment"), each = 30)),
#'   patient = factor(rep(1:30, 2)),
#'   response = c(rnorm(30, 100, 20), rnorm(30, 120, 20))
#' )
#'
#' # Paired violin with connecting lines
#' ViolinPlot(paired_data,
#'   x = "condition", y = "response",
#'   paired_by = "patient",
#'   add_box = TRUE
#' )
#'
#' # With paired t-test
#' ViolinPlot(paired_data,
#'   x = "condition", y = "response",
#'   paired_by = "patient",
#'   comparisons = list(c("Baseline", "Treatment")),
#'   pairwise_method = "t.test"
#' )
#'
#' # ============================================================
#' # Highlighting and Visual Enhancements
#' # ============================================================
#'
#' # Highlight extreme values
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   add_point = TRUE,
#'   highlight = "value > 80 | value < 10",
#'   highlight_color = "red",
#'   highlight_size = 2
#' )
#'
#' # Add trend line
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   add_trend = TRUE,
#'   trend_linewidth = 1.5
#' )
#'
#' # Add reference line
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   add_line = 50,
#'   line_color = "darkgreen",
#'   line_type = 1
#' )
#'
#' # Add mean indicator
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   add_stat = mean,
#'   stat_name = "Mean",
#'   stat_color = "red",
#'   stat_shape = 18,
#'   stat_size = 3
#' )
#'
#' # ============================================================
#' # Fill Modes
#' # ============================================================
#'
#' # Fill by x category
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   fill_mode = "x",
#'   palette = "Pastel1"
#' )
#'
#' # Fill by mean (gradient coloring)
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   fill_mode = "mean",
#'   palette = "RdYlGn"
#' )
#'
#' # Fill by median with reversed gradient
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   fill_mode = "median",
#'   palette = "Blues",
#'   fill_reverse = TRUE
#' )
#'
#' # ============================================================
#' # Sorting and Orientation
#' # ============================================================
#'
#' # Sort by mean value
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   sort_x = "mean_desc",
#'   add_box = TRUE
#' )
#'
#' # Horizontal violin plot
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   flip = TRUE,
#'   add_box = TRUE
#' )
#'
#' # ============================================================
#' # Faceting
#' # ============================================================
#'
#' # Add faceting variable
#' data$experiment <- sample(c("Exp1", "Exp2"), nrow(data), replace = TRUE)
#'
#' # Facet by experiment
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   facet_by = "experiment",
#'   add_box = TRUE
#' )
#'
#' # Free scales
#' ViolinPlot(data,
#'   x = "category", y = "value",
#'   facet_by = "experiment",
#'   facet_scales = "free_y"
#' )
#'
#' # ============================================================
#' # Wide Format Data
#' # ============================================================
#'
#' # Wide format input
#' wide_data <- data.frame(
#'   Control = rnorm(50, 100, 15),
#'   LowDose = rnorm(50, 110, 15),
#'   HighDose = rnorm(50, 130, 20)
#' )
#'
#' ViolinPlot(wide_data,
#'   x = c("Control", "LowDose", "HighDose"),
#'   in_form = "wide",
#'   add_box = TRUE,
#'   xlab = "Treatment",
#'   ylab = "Response"
#' )
#'
#' # ============================================================
#' # Complex Example: Publication-Ready Plot
#' # ============================================================
#'
#' # Gene expression data example
#' expr_data <- data.frame(
#'   gene = rep(c("BRCA1", "TP53", "EGFR", "MYC"), each = 40),
#'   expression = c(
#'     rnorm(40, 8, 1.5),
#'     rnorm(40, 6, 2),
#'     rnorm(40, 10, 1),
#'     rnorm(40, 12, 2.5)
#'   ),
#'   tissue = rep(rep(c("Normal", "Tumor"), each = 20), 4)
#' )
#'
#' ViolinPlot(expr_data,
#'   x = "gene",
#'   y = "expression",
#'   group_by = "tissue",
#'   add_box = TRUE,
#'   add_point = TRUE,
#'   pt_alpha = 0.4,
#'   comparisons = TRUE,
#'   sig_label = "p.signif",
#'   hide_ns = TRUE,
#'   palette = c("Normal" = "#4DAF4A", "Tumor" = "#E41A1C"),
#'   title = "Gene Expression by Tissue Type",
#'   xlab = "Gene",
#'   ylab = "Expression (log2)",
#'   legend.position = "bottom"
#' )
#' }
ViolinPlot <- function(
    data, x, x_sep = "_", y = NULL, in_form = c("long", "wide"),
    split_by = NULL, split_by_sep = "_", symnum_args = NULL,
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    paired_by = NULL, x_text_angle = NULL, step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_ggforge", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = "grey30", pt_size = NULL, pt_alpha = 1,
    jitter_width = NULL, jitter_height = 0, stack = FALSE, y_max = NULL, y_min = NULL,
    add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = "p.format", sig_labelsize = 3.5, hide_ns = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, ...) {
  stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")

  BoxViolinPlot(
    data = data, x = x, x_sep = x_sep, y = y, base = "violin", in_form = in_form,
    split_by = split_by, split_by_sep = split_by_sep,
    sort_x = sort_x, flip = flip, keep_empty = keep_empty,
    group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
    paired_by = paired_by, x_text_angle = x_text_angle,
    fill_mode = fill_mode, fill_reverse = fill_reverse, step_increase = step_increase,
    theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
    aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
    add_point = add_point, pt_color = pt_color, pt_size = pt_size, pt_alpha = pt_alpha,
    symnum_args = symnum_args,
    jitter_width = jitter_width, jitter_height = jitter_height, stack = stack,
    y_max = y_max, y_min = y_min,
    add_box = add_box, box_color = box_color, box_width = box_width, box_ptsize = box_ptsize,
    add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth,
    trend_ptsize = trend_ptsize,
    add_stat = add_stat, stat_name = stat_name, stat_color = stat_color,
    stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
    add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
    add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
    highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size,
    highlight_alpha = highlight_alpha,
    comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
    multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
    sig_label = sig_label, sig_labelsize = sig_labelsize, hide_ns = hide_ns,
    facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol,
    facet_nrow = facet_nrow, facet_byrow = facet_byrow,
    title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed,
    combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
    axes = axes, axis_titles = axis_titles, guides = guides, ...
  )
}

#' Add paired lines to plot
#'
#' @description
#' Adds connecting lines between paired observations in box/violin plots.
#' Supports both simple pairing (connecting across x categories) and
#' grouped pairing (connecting within x categories across groups).
#'
#' @param p ggplot object to modify
#' @param data Data frame containing the plot data
#' @param x Character string; x-axis column name
#' @param y Character string; y-axis column name
#' @param paired_by Character string; pairing column name
#' @param group_by Character string; grouping column name (can be NULL)
#' @param pt_color Character string; line color
#' @param pt_alpha Numeric; line transparency (0-1)
#'
#' @return Modified ggplot object with paired lines added
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Simple paired data (before/after)
#' df <- data.frame(
#'   time = factor(rep(c("Before", "After"), each = 10)),
#'   subject = factor(rep(1:10, 2)),
#'   value = c(rnorm(10, 100), rnorm(10, 95))
#' )
#'
#' p <- ggplot(df, aes(x = time, y = value)) +
#'   geom_boxplot()
#'
#' # Add paired lines (internal function call)
#' p <- add_paired_lines(p, df, "time", "value", "subject", NULL, "grey50", 0.5)
#' }
#'
#' @keywords internal
#' @importFrom ggplot2 geom_line aes
#' @importFrom rlang sym
add_paired_lines <- function(p, data, x, y, paired_by, group_by, pt_color, pt_alpha) {
  if (is.null(group_by)) {
    # Simple paired lines (no grouping)
    p + ggplot2::geom_line(
      data = data,
      mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), group = !!rlang::sym(paired_by)),
      color = pt_color,
      alpha = pt_alpha,
      linewidth = 0.3,
      inherit.aes = FALSE
    )
  } else {
    # Paired lines with grouping (dodged positions)
    line_data <- data
    line_data$.xint <- as.numeric(line_data[[x]])
    groups <- levels(line_data[[group_by]])
    line_data$.x <- ifelse(
      line_data[[group_by]] == groups[1],
      line_data$.xint - .225,
      line_data$.xint + .225
    )
    line_data$.line_group <- paste(line_data[[paired_by]], line_data[[x]], sep = " // ")

    p + ggplot2::geom_line(
      data = line_data,
      mapping = ggplot2::aes(x = !!rlang::sym(".x"), y = !!rlang::sym(y), group = !!rlang::sym(".line_group")),
      color = pt_color,
      alpha = pt_alpha,
      linewidth = 0.3,
      inherit.aes = FALSE
    )
  }
}

# ============================================================================
# Main Atomic Function
# ============================================================================

#' Box/Violin Plot Atomic
#'
#' @description
#' Creates a single box or violin plot without splitting. This is the core
#' plotting function that handles all the actual plot construction.
#'
#' @details
#' This internal function is responsible for:
#' - Data preparation and validation
#' - Building the base ggplot object
#' - Adding layers (points, trend lines, statistical comparisons)
#' - Applying themes and formatting
#' - Calculating optimal dimensions
#'
#' For typical usage, use the wrapper functions `BoxPlot()` or `ViolinPlot()`
#' instead.
#'
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_violin geom_jitter geom_point geom_line
#' @importFrom ggplot2 geom_hline geom_vline layer_data scale_fill_manual scale_color_manual
#' @importFrom ggplot2 scale_shape_manual scale_linetype_manual stat_summary scale_fill_gradientn
#' @importFrom ggplot2 labs theme element_line element_text position_dodge position_jitter
#' @importFrom ggplot2 coord_flip coord_cartesian layer_scales position_jitterdodge
#' @importFrom ggplot2 scale_shape_identity scale_size_manual scale_alpha_manual
#' @importFrom ggplot2 scale_y_continuous scale_x_discrete guide_colorbar guide_legend unit
#' @importFrom ggnewscale new_scale_fill
#' @importFrom ggpubr geom_pwc stat_compare_means
#' @importFrom dplyr mutate ungroup first arrange group_by summarise n
#' @importFrom tidyr pivot_longer
#' @importFrom rlang sym syms parse_expr "%||%"
#' @importFrom utils combn
#' @importFrom stats median quantile
BoxViolinPlotAtomic <- function(
    data, x, x_sep = "_", y = NULL, base = c("box", "violin"), in_form = c("long", "wide"),
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    paired_by = NULL, x_text_angle = NULL, step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE, symnum_args = NULL,
    theme = "theme_ggforge", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = "grey30", pt_size = NULL, pt_alpha = 1, y_nbreaks = 4,
    jitter_width = NULL, jitter_height = 0, stack = FALSE, y_max = NULL, y_min = NULL, y_trans = "identity",
    add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = "p.format", sig_labelsize = 3.5, hide_ns = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525, ...) {
  set.seed(seed)

  # Get ggplot function (with gglogger support)
  ggplot <- get_ggplot()

  # Extract base size for text scaling
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  text_size_scale <- base_size / 12

  # ========== Data Preparation ==========

  # Convert wide format to long format if needed
  in_form <- match.arg(in_form)
  if (in_form == "wide") {
    data <- data %>% tidyr::pivot_longer(cols = x, names_to = ".x", values_to = ".y")
    x <- ".x"
    y <- ".y"
  }

  # Validate and process columns
  x <- validate_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
  y <- validate_columns(data, y)
  group_by <- validate_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
  facet_by <- validate_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
  paired_by <- validate_columns(data, paired_by, force_factor = TRUE)

  # Paired data validation
  if (!is.null(paired_by)) {
    if (!isTRUE(add_point)) {
      warning("Forcing 'add_point' = TRUE when 'paired_by' is provided.")
      add_point <- TRUE
    }
    data <- validate_paired_data(data, x, y, paired_by, group_by)
  }

  # Setup for pairwise comparisons
  if (isTRUE(comparisons) && is.null(group_by)) {
    comparisons <- utils::combn(levels(data[[x]]), 2, simplify = FALSE)
  }

  if (length(comparisons) > 0) {
    if (!is.list(comparisons) && !isTRUE(comparisons)) {
      comparisons <- list(comparisons)
    }
    ncomp <- sapply(comparisons, length)
    if (any(ncomp > 2)) {
      stop("'comparisons' must be a list in which all elements must be vectors of length 2")
    }
  }

  if (!isFALSE(multiplegroup_comparisons)) {
    stopifnot(
      "'sig_label' must be 'p.format' or 'p.signif' when 'multiplegroup_comparisons' is TRUE." =
        sig_label %in% c("p.format", "p.signif")
    )
  }

  # Sort x-axis
  sort_x <- match.arg(sort_x)
  if (sort_x != "none" && !is.null(facet_by)) {
    stop("Cannot sort x-axis when facet_by is provided.")
  }

  data <- data %>%
    dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
    mutate(.y_mean = mean(!!sym(y)), .y_median = median(!!sym(y))) %>%
    ungroup()

  # Calculate y-axis limits (support quantile notation like "q95")
  values <- data[[y]][is.finite(data[[y]])]
  if (is.character(y_max)) {
    q_max <- as.numeric(sub("(^q)(\\d+)", "\\2", y_max)) / 100
    y_max_use <- stats::quantile(values, q_max, na.rm = TRUE)
  } else {
    y_max_use <- max(values, na.rm = TRUE)
  }

  if (is.null(y_min)) {
    y_min_use <- min(values, na.rm = TRUE)
  } else if (is.character(y_min)) {
    q_min <- as.numeric(sub("(^q)(\\d+)", "\\2", y_min)) / 100
    y_min_use <- stats::quantile(values, q_min, na.rm = TRUE)
  } else {
    y_min_use <- y_min
  }
  rm(values)

  # Setup highlight indicators for points
  if (!is.null(highlight)) {
    if (isTRUE(highlight)) {
      data$.highlight <- TRUE
    } else if (is.numeric(highlight)) {
      data$.highlight <- seq_len(nrow(data)) %in% highlight
    } else if (is.character(highlight) && length(highlight) == 1) {
      data <- dplyr::mutate(data, .highlight = !!rlang::parse_expr(highlight))
    } else if (is.null(rownames(data))) {
      stop("No row names in the data, please provide a vector of indexes to highlight.", call. = FALSE)
    } else {
      data$.highlight <- rownames(data) %in% highlight
    }
    if (isFALSE(add_point)) {
      warning("Forcing add_point = TRUE when highlight is provided.")
      add_point <- TRUE
    }
  } else {
    data$.highlight <- FALSE
  }
  data$.highlight <- factor(as.character(data$.highlight), levels = c("TRUE", "FALSE"))

  # Apply x-axis sorting
  if (sort_x == "mean" || sort_x == "mean_asc") {
    data[[x]] <- stats::reorder(data[[x]], data$.y_mean)
  } else if (sort_x == "mean_desc") {
    data[[x]] <- stats::reorder(data[[x]], -data$.y_mean)
  } else if (sort_x == "median" || sort_x == "median_asc") {
    data[[x]] <- stats::reorder(data[[x]], data$.y_median)
  } else if (sort_x == "median_desc") {
    data[[x]] <- stats::reorder(data[[x]], -data$.y_median)
  }

  # Flip x-axis if needed
  if (isTRUE(flip)) {
    data[[x]] <- factor(data[[x]], levels = rev(levels(data[[x]])))
    aspect.ratio <- 1 / aspect.ratio
    if (length(aspect.ratio) == 0 || is.na(aspect.ratio)) {
      aspect.ratio <- NULL
    }
  }

  # Auto-detect x_text_angle if not specified
  if (is.null(x_text_angle)) {
    nd <- ifelse(is.null(group_by), 1, nlevels(data[[group_by]]))
    x_text_angle <- auto_detect_text_angle(data, x, flip, stack, nd)
  }

  # ========== Plot Construction ==========

  # Validate base plot type
  base <- match.arg(base)
  if (isTRUE(add_box) && base == "box") {
    stop("Cannot add box plot to box plot. Use 'base = \"violin\"' with 'add_box = TRUE'.", call. = FALSE)
  }

  # Determine fill aesthetic based on fill_mode
  fill_mode <- match.arg(fill_mode, c("dodge", "x", "mean", "median"))
  fill_by <- switch(fill_mode,
    dodge = group_by,
    x = x,
    mean = ".y_mean",
    median = ".y_median"
  )

  # Initialize ggplot with base aesthetics
  p <- ggplot(data, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), fill = !!rlang::sym(fill_by)))

  # Add background layer (if requested)
  if (isTRUE(add_bg)) {
    p <- p + create_bg_layer(data, x, bg_palette, bg_palcolor, bg_alpha, keep_empty, facet_by)
  }

  # Add base layer (box or violin)
  if (base == "box") {
    p <- p + ggplot2::geom_boxplot(
      position = ggplot2::position_dodge(width = 0.9),
      color = "black",
      width = 0.8,
      outlier.shape = NA
    )
  } else {
    p <- p + ggplot2::geom_violin(
      position = ggplot2::position_dodge(width = 0.9),
      scale = "width",
      trim = TRUE,
      alpha = alpha,
      width = 0.8
    )
  }

  # Add fill scale based on fill_mode
  if (fill_mode == "dodge") {
    # Discrete colors by group
    p <- p + ggplot2::scale_fill_manual(
      name = group_name %||% group_by,
      values = get_palette(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
    )
  } else if (fill_mode == "x") {
    # Discrete colors by x
    p <- p + ggplot2::scale_fill_manual(
      name = x,
      values = get_palette(levels(data[[x]]), palette = palette, palcolor = palcolor)
    )
  } else {
    # Continuous gradient by mean/median
    p <- p + ggplot2::scale_fill_gradientn(
      name = paste0(y, " (", fill_mode, ")"),
      n.breaks = 3,
      colors = get_palette(palette = palette, palcolor = palcolor, reverse = fill_reverse),
      na.value = "grey80",
      guide = ggplot2::guide_colorbar(frame.colour = "black", ticks.colour = "black", frame.linewidth = 0.3, ticks.linewidth = 0.3, title.hjust = 0)
    )
  }

  # Add box overlay (for violin plots)
  if (isTRUE(add_box)) {
    p <- p +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_boxplot(
        position = ggplot2::position_dodge(width = 0.9),
        fill = box_color,
        color = box_color,
        width = box_width,
        show.legend = FALSE,
        outlier.shape = NA
      ) +
      ggplot2::stat_summary(
        fun = dplyr::first,
        geom = "point",
        mapping = ggplot2::aes(y = !!rlang::sym(".y_median")),
        position = ggplot2::position_dodge(width = 0.9),
        color = "black",
        fill = "white",
        size = box_ptsize,
        shape = 21
      )
  }

  # ========== Statistical Comparisons ==========

  if (length(comparisons) > 0) {
    if (isTRUE(comparisons)) {
      # Auto-detect if multiple group comparison is needed
      if (any(rowSums(table(data[[x]], data[[group_by]]) >= 2) >= 3)) {
        message("Detected more than 2 groups. Using multiple_method for comparison")
        multiplegroup_comparisons <- TRUE
      } else {
        method <- pairwise_method

        # Ensure fill_mode is compatible with pairwise comparisons
        if (!identical(fill_mode, "dodge")) {
          stop("'comparisons' can only be used with 'fill_mode = \"dodge\"'.", call. = FALSE)
        }

        # Preprocess data for comparisons
        pwc_data <- preprocess_comparison_data_grouped(data, x, y, group_by, facet_by)

        # Add paired test support when paired_by is provided
        pwc_call <- list(
          data = pwc_data,
          label = sig_label,
          label.size = text_size_scale * sig_labelsize,
          y.position = y_max_use,
          step.increase = step_increase,
          symnum.args = symnum_args,
          tip.length = 0,
          vjust = 0,
          ref.group = ref_group,
          method = method,
          hide.ns = hide_ns
        )

        if (!is.null(paired_by)) {
          pwc_call$method.args <- c(pwc_call$method.args, list(paired = TRUE))
        }

        p <- p + do.call(ggpubr::geom_pwc, pwc_call)
        y_max_use <- layer_scales(p)$y$range$range[2]
      }
    } else if (!isTRUE(multiplegroup_comparisons)) {
      # Validate that group_by is NULL for simple pairwise comparisons
      if (!is.null(group_by)) {
        stop("'comparisons' can only be used when 'group_by' is NULL.", call. = FALSE)
      }

      # Convert comparisons to indices
      comparisons <- lapply(
        comparisons,
        function(el) {
          if (!is.numeric(el)) {
            which(levels(data[[x]]) %in% el)
          } else {
            el
          }
        }
      )

      # Preprocess data for comparisons
      pwc_data <- preprocess_comparison_data_simple(data, x, y, facet_by)

      # Add paired test support when paired_by is provided
      method_args <- list(comparisons = comparisons)
      if (!is.null(paired_by)) {
        method_args$paired <- TRUE
      }

      p <- p + ggpubr::geom_pwc(
        data = pwc_data,
        label = sig_label,
        label.size = text_size_scale * sig_labelsize,
        y.position = y_max_use,
        step.increase = step_increase,
        symnum.args = symnum_args,
        tip.length = 0,
        vjust = 0,
        ref.group = ref_group,
        method = pairwise_method,
        method.args = method_args,
        hide.ns = hide_ns
      )
      y_max_use <- layer_scales(p)$y$range$range[1] +
        (layer_scales(p)$y$range$range[2] - layer_scales(p)$y$range$range[1]) * 1.15
    }
  }

  # Multiple group comparisons
  if (isTRUE(multiplegroup_comparisons)) {
    p <- p + ggpubr::stat_compare_means(
      mapping = if (!is.null(group_by)) {
        aes(x = !!sym(x), y = !!sym(y), group = !!sym(group_by))
      } else {
        aes(x = !!sym(x), y = !!sym(y))
      },
      inherit.aes = FALSE,
      method = multiple_method,
      symnum.args = symnum_args,
      label.y = y_max_use,
      size = text_size_scale * sig_labelsize,
      label = sig_label,
      tip.length = 0,
      vjust = -0.5,
      hjust = ifelse(is.null(group_by), 0, 0.5)
    )
    y_max_use <- layer_scales(p)$y$range$range[1] +
      (layer_scales(p)$y$range$range[2] - layer_scales(p)$y$range$range[1]) * 1.15
  }

  if (!is.null(y_max) && is.numeric(y_max)) {
    y_max_use <- max(y_max_use, y_max)
  }

  # ========== Additional Layers ==========

  # Add jittered points (with optional paired lines)
  if (isTRUE(add_point)) {
    # Add paired lines first (if applicable)
    if (!is.null(paired_by)) {
      p <- add_paired_lines(p, data, x, y, paired_by, group_by, pt_color, pt_alpha)
    }

    # Add points with highlight support
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(
          fill = !!rlang::sym(fill_by),
          color = !!rlang::sym(".highlight"),
          size = !!rlang::sym(".highlight"),
          alpha = !!rlang::sym(".highlight")
        ),
        position = ggplot2::position_jitterdodge(
          jitter.width = jitter_width %||% ifelse(!is.null(paired_by), 0, 0.5),
          jitter.height = jitter_height,
          dodge.width = 0.9,
          seed = seed
        ),
        show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(values = c("TRUE" = highlight_color, "FALSE" = pt_color)) +
      ggplot2::scale_size_manual(values = c("TRUE" = highlight_size, "FALSE" = pt_size %||% min(3000 / nrow(data), 0.6))) +
      ggplot2::scale_alpha_manual(values = c("TRUE" = highlight_alpha, "FALSE" = pt_alpha))
  }

  # Add trend line connecting medians
  if (isTRUE(add_trend)) {
    if (is.null(trend_color)) {
      # Use palette colors for trend lines
      p <- p + ggplot2::stat_summary(
        fun = dplyr::first,
        geom = "line",
        mapping = if (!is.null(group_by)) {
          ggplot2::aes(y = !!rlang::sym(".y_median"), group = !!rlang::sym(group_by), color = !!rlang::sym(group_by))
        } else {
          ggplot2::aes(y = !!rlang::sym(".y_median"), group = 1)
        },
        position = ggplot2::position_dodge(width = 0.9),
        linewidth = trend_linewidth
      )
      if (!is.null(group_by)) {
        p <- p + ggplot2::scale_color_manual(
          values = get_palette(levels(data[[group_by]]), palette = palette, palcolor = palcolor),
          guide = "none"
        )
      }
    } else {
      # Use fixed color for trend lines
      p <- p + ggplot2::stat_summary(
        fun = dplyr::first,
        geom = "line",
        mapping = if (!is.null(group_by)) {
          ggplot2::aes(y = !!rlang::sym(".y_median"), group = !!rlang::sym(group_by))
        } else {
          ggplot2::aes(y = !!rlang::sym(".y_median"), group = 1)
        },
        position = ggplot2::position_dodge(width = 0.9),
        color = trend_color,
        linewidth = trend_linewidth
      )
    }

    # Add points at medians
    p <- p + ggplot2::stat_summary(
      fun = dplyr::first,
      geom = "point",
      mapping = if (!is.null(group_by)) {
        ggplot2::aes(y = !!rlang::sym(".y_median"), group = !!rlang::sym(group_by))
      } else {
        ggplot2::aes(y = !!rlang::sym(".y_median"), group = 1)
      },
      position = ggplot2::position_dodge(width = 0.9),
      color = "black",
      fill = "white",
      size = trend_ptsize,
      shape = 21
    )
  }

  # Add horizontal reference line
  if (!is.null(add_line)) {
    p <- p + ggplot2::geom_hline(
      yintercept = add_line,
      color = line_color,
      linetype = line_type,
      linewidth = line_width
    )
  }

  # Add custom statistical summary (e.g., mean)
  if (!is.null(add_stat)) {
    p <- p + ggplot2::stat_summary(
      fun = add_stat,
      geom = "point",
      mapping = if (!is.null(group_by)) {
        ggplot2::aes(shape = !!rlang::sym("stat_shape"), group = !!rlang::sym(group_by))
      } else {
        ggplot2::aes(shape = !!rlang::sym("stat_shape"), group = 1)
      },
      position = ggplot2::position_dodge(width = 0.9),
      color = stat_color,
      fill = stat_color,
      size = stat_size,
      stroke = stat_stroke
    ) + ggplot2::scale_shape_identity(
      labels = stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")"),
      guide = ggplot2::guide_legend(title = "", order = 2)
    )
  }

  # ========== Finalize Plot ==========

  # Calculate text justification based on angle
  just <- calc_justification(x_text_angle)

  # Add scales and labels
  p <- p +
    ggplot2::scale_x_discrete(drop = !keep_empty) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
    ggplot2::scale_y_continuous(trans = y_trans, n.breaks = y_nbreaks)

  # Calculate layout
  layout_info <- calculate_plot_layout(
    data, x, group_by, flip, stack, facet_by, facet_scales,
    facet_nrow, facet_ncol, legend.position, legend.direction,
    y_min_use, y_max_use
  )

  # Apply coordinate system
  if (!is.null(layout_info$coord)) {
    p <- p + layout_info$coord
  }

  # Apply theme
  p <- apply_boxviolin_theme(
    p, theme, theme_args, layout_info, aspect.ratio,
    legend.position, legend.direction, x_text_angle, just,
    flip, stack, data, x, y
  )

  # Set plot dimensions
  attr(p, "height") <- layout_info$height
  attr(p, "width") <- layout_info$width

  # Apply faceting
  p <- add_facets(p, facet_by, facet_scales, layout_info$facet_nrow, layout_info$facet_ncol, facet_byrow,
    strip.position = layout_info$strip_position
  )

  return(p)
}
