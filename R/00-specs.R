#' ggforge Style Specifications System
#'
#' @description
#' Centralized style specifications for all visual elements.
#' This is the authoritative source for all styling rules in ggforge.
#'
#' @name style-specs
#' @keywords internal
NULL

# =============================================================================
# STYLE SPECIFICATIONS - Single Source of Truth
# =============================================================================

#' Core style specifications
#' @keywords internal
.ggforge_style_specs <- list(
  # Font specifications
  font = list(
    # Plot title
    title = list(
      size = 13,
      face = "bold",
      hjust = 0.5,
      colour = "black"
    ),

    # Axis titles
    axis_title = list(
      size = 12,
      face = "bold",
      colour = "black"
    ),

    # Axis text - varies by variable type
    axis_text = list(
      continuous = list(
        size = 10,
        colour = "black"
      ),
      discrete = list(
        size = 12,
        colour = "black"
      )
    ),

    # Facet strip text
    facet_text = list(
      size = 12,
      face = "bold",
      colour = "black"
    ),

    # Legend title
    legend_title = list(
      size = 12,
      face = "bold",
      colour = "black"
    ),

    # Legend text
    legend_text = list(
      size = 10,
      colour = "black"
    )
  ),

  # Line specifications
  lines = list(
    base_size = 0.6,
    grid_major = list(
      colour = "grey80",
      linetype = 2,
      linewidth = 0.5
    ),
    grid_minor = list(
      colour = "grey90",
      linetype = 2,
      linewidth = 0.3
    )
  ),

  # Spacing specifications
  spacing = list(
    plot_margin = c(10, 10, 10, 10),
    panel_spacing = 5
  )
)

#' Get style specification by path
#'
#' @description
#' Retrieves a style specification using dot-notation path.
#' Automatically scales font sizes based on base_size.
#'
#' @param path Character. Dot-separated path (e.g., "font.axis_text.continuous")
#' @param base_size Numeric. Base font size for scaling (default: 12)
#' @return The specification value (list or atomic)
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_style_spec("font.title")
#' get_style_spec("font.axis_text.continuous", base_size = 14)
#' }
get_style_spec <- function(path, base_size = 12) {
  # Split path by dots (fixed = TRUE means literal dot, no need for \\)
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]

  # Get specs - direct access works in both installed and dev mode
  spec <- .ggforge_style_specs

  # Navigate to the specification
  for (part in parts) {
    if (is.null(spec[[part]])) {
      stop("Style specification not found: ", path,
        "\nAvailable at this level: ", paste(names(spec), collapse = ", "),
        call. = FALSE
      )
    }
    spec <- spec[[part]]
  }

  # Scale font sizes if present
  if (is.list(spec) && "size" %in% names(spec)) {
    spec$size <- spec$size * base_size / 12
  }

  return(spec)
}

#' Build element_text from style spec
#'
#' @description
#' Converts a style specification to ggplot2::element_text()
#'
#' @param spec_path Character. Path to style specification
#' @param base_size Numeric. Base font size
#' @param ... Additional arguments to override spec
#' @return A ggplot2::element_text object
#' @keywords internal
build_element_text <- function(spec_path, base_size = 12, ...) {
  spec <- get_style_spec(spec_path, base_size)

  # Merge with user overrides
  overrides <- list(...)
  if (length(overrides) > 0) {
    spec <- utils::modifyList(spec, overrides)
  }

  do.call(ggplot2::element_text, spec)
}

#' Build element_line from style spec
#'
#' @description
#' Converts a style specification to ggplot2::element_line()
#'
#' @param spec_path Character. Path to style specification
#' @param ... Additional arguments to override spec
#' @return A ggplot2::element_line object
#' @keywords internal
build_element_line <- function(spec_path, ...) {
  spec <- get_style_spec(spec_path)

  # Merge with user overrides
  overrides <- list(...)
  if (length(overrides) > 0) {
    spec <- utils::modifyList(spec, overrides)
  }

  do.call(ggplot2::element_line, spec)
}
