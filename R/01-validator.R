#' Parameter Validation System
#'
#' @description
#' A robust validation system for plot parameters with clear error messages
#' and automatic type coercion where appropriate.
#'
#' @name validator
#' @keywords internal
NULL

#' Column Validator
#'
#' @description
#' Validates that specified columns exist in a data frame and optionally
#' converts them to factors with proper level ordering.
#'
#' @param data A data frame
#' @param columns Column name(s) to validate
#' @param force_factor Whether to convert to factor
#' @param allow_multi Whether to allow multiple columns
#' @param concat_multi Whether to concatenate multiple columns
#' @param concat_sep Separator for concatenation
#' @return Validated column name(s)
#' @keywords internal
#' @importFrom tidyr unite
#' @importFrom rlang sym syms
#' @importFrom tidyr expand_grid
validate_columns <- function(
    data,
    columns,
    force_factor = FALSE,
    allow_multi = FALSE,
    concat_multi = FALSE,
    concat_sep = "_") {
  if (is.null(data)) {
    stop("Data is NULL", call. = FALSE)
  }

  if (is.null(columns)) {
    return(NULL)
  }

  # Get calling context for better error messages
  param_name <- deparse(substitute(columns))
  df_name <- deparse(substitute(data))

  # Check if multiple columns are allowed
  if (!allow_multi && length(columns) > 1) {
    stop(
      sprintf("Only one column allowed in '%s', got %d", param_name, length(columns)),
      call. = FALSE
    )
  }

  # Check all columns exist
  missing_cols <- setdiff(columns, colnames(data))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Column(s) not found in data: %s",
        paste0("'", missing_cols, "'", collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Handle multiple column concatenation
  if (allow_multi && concat_multi && length(columns) > 1) {
    message(
      sprintf(
        "Concatenating %d columns in '%s' with separator '%s'",
        length(columns), param_name, concat_sep
      )
    )

    new_col <- paste(columns, collapse = concat_sep)
    data <- unite(data, !!sym(new_col), !!!syms(columns), sep = concat_sep, remove = FALSE)

    # Preserve factor level ordering if needed
    if (force_factor) {
      all_levels <- lapply(columns, function(col) {
        if (is.factor(data[[col]])) {
          levels(data[[col]])
        } else {
          unique(data[[col]])
        }
      })
      all_levels <- do.call(expand_grid, all_levels)
      all_levels <- apply(all_levels, 1, paste, collapse = concat_sep)
      data[[new_col]] <- droplevels(factor(data[[new_col]], levels = unique(all_levels)))
    }

    columns <- new_col
  }

  # Force to factor if requested
  if (force_factor) {
    parent_env <- parent.frame()
    for (col in columns) {
      if (!is.factor(data[[col]])) {
        parent_env[[df_name]][[col]] <- factor(data[[col]], levels = unique(data[[col]]))
      } else if (!col %in% colnames(parent_env[[df_name]])) {
        parent_env[[df_name]][[col]] <- data[[col]]
      }
    }
  }

  return(columns)
}

#' Validate and normalize common plot arguments
#'
#' @description
#' Validates common arguments used across all plot types
#'
#' @param seed Random seed
#' @param facet_by Faceting columns
#' @param split_by Split columns
#' @param group_by Grouping columns
#' @param facet_scales Facet scales type
#' @param theme Theme name or function
#' @param palette Palette name
#' @param alpha Transparency value
#' @param aspect.ratio Aspect ratio
#' @param legend.position Legend position
#' @param legend.direction Legend direction
#' @param ... Additional arguments (ignored)
#' @return Invisible NULL (sets seed as side effect)
#' @keywords internal
validate_common_args <- function(
    seed = 8525,
    facet_by = NULL,
    split_by = NULL,
    group_by = NULL,
    facet_scales = "fixed",
    theme = "theme_ggforge",
    palette = "Paired",
    alpha = 1,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    ...) {
  # Validate seed
  if (!is.numeric(seed) || length(seed) != 1) {
    stop("'seed' must be a single numeric value", call. = FALSE)
  }
  set.seed(seed)

  # Validate facet_by
  if (length(facet_by) > 2) {
    stop("Maximum 2 columns allowed in 'facet_by'", call. = FALSE)
  }

  # Validate facet_scales
  valid_scales <- c("fixed", "free", "free_x", "free_y")
  if (!facet_scales %in% valid_scales) {
    stop(
      sprintf(
        "'facet_scales' must be one of: %s",
        paste(valid_scales, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Validate alpha
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("'alpha' must be a numeric value between 0 and 1", call. = FALSE)
  }

  # Validate aspect.ratio
  if (!is.null(aspect.ratio) && (!is.numeric(aspect.ratio) || aspect.ratio <= 0)) {
    stop("'aspect.ratio' must be a positive numeric value", call. = FALSE)
  }

  # Validate legend.position (handle vectors for split_by)
  valid_positions <- c("none", "left", "right", "bottom", "top")
  if (is.character(legend.position) && length(legend.position) == 1) {
    if (!legend.position %in% valid_positions) {
      stop(
        sprintf(
          "'legend.position' must be one of: %s",
          paste(valid_positions, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  } else if (is.character(legend.position) && length(legend.position) > 1) {
    # Validate each element for vectors (used with split_by)
    invalid <- legend.position[!legend.position %in% valid_positions]
    if (length(invalid) > 0) {
      stop(
        sprintf(
          "Invalid 'legend.position' value(s): %s. Must be one of: %s",
          paste(invalid, collapse = ", "),
          paste(valid_positions, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # Validate legend.direction (handle vectors for split_by)
  valid_directions <- c("horizontal", "vertical")
  if (length(legend.direction) == 1) {
    if (!legend.direction %in% valid_directions) {
      stop(
        sprintf(
          "'legend.direction' must be one of: %s",
          paste(valid_directions, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  } else if (length(legend.direction) > 1) {
    # Validate each element for vectors (used with split_by)
    invalid <- legend.direction[!legend.direction %in% valid_directions]
    if (length(invalid) > 0) {
      stop(
        sprintf(
          "Invalid 'legend.direction' value(s): %s. Must be one of: %s",
          paste(invalid, collapse = ", "),
          paste(valid_directions, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}

#' Validate numeric range
#'
#' @param value Value to validate
#' @param param_name Parameter name for error message
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#' @param allow_null Whether NULL is allowed
#' @return The validated value
#' @keywords internal
validate_numeric_range <- function(
    value,
    param_name,
    min_val = -Inf,
    max_val = Inf,
    allow_null = TRUE) {
  if (is.null(value)) {
    if (allow_null) {
      return(NULL)
    } else {
      stop(sprintf("'%s' cannot be NULL", param_name), call. = FALSE)
    }
  }

  if (!is.numeric(value)) {
    stop(sprintf("'%s' must be numeric", param_name), call. = FALSE)
  }

  if (any(value < min_val | value > max_val, na.rm = TRUE)) {
    stop(
      sprintf("'%s' must be between %s and %s", param_name, min_val, max_val),
      call. = FALSE
    )
  }

  return(value)
}

#' Validate choice from options
#'
#' @param value Value to validate
#' @param param_name Parameter name
#' @param choices Valid choices
#' @param allow_null Whether NULL is allowed
#' @return The validated value
#' @keywords internal
validate_choice <- function(
    value,
    param_name,
    choices,
    allow_null = TRUE) {
  if (is.null(value)) {
    if (allow_null) {
      return(NULL)
    } else {
      stop(sprintf("'%s' cannot be NULL", param_name), call. = FALSE)
    }
  }

  if (!value %in% choices) {
    stop(
      sprintf(
        "'%s' must be one of: %s",
        param_name,
        paste(choices, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  return(value)
}
