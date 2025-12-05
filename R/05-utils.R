#' Utility Functions for ggforge
#'
#' @description
#' Helper functions for data manipulation, layout, and plotting
#'
#' @name utils
#' @keywords internal
NULL

# NOTE: get_axis_text_size() has been replaced by apply_style_theme()
# See R/03-themes.R for the new implementation based on the style specs system

#' Check linewidth parameter (compatibility shim for qqplotr)
#'
#' @description
#' This is a compatibility shim for the qqplotr package which expects
#' the check_linewidth() function from older versions of ggplot2.
#' This function handles the conversion of the old 'size' aesthetic to
#' the new 'linewidth' aesthetic.
#'
#' @param data Data frame containing aesthetic data
#' @param snake_class Class name in snake_case (unused, for compatibility)
#' @return Data frame with size converted to linewidth if needed
#' @keywords internal
#' @export
check_linewidth <- function(data, snake_class = NULL) {
  # This is a compatibility shim for the old ggplot2 internal function
  # that was used to convert 'size' to 'linewidth'

  # If data has a 'size' column but no 'linewidth', convert it
  if (!is.null(data$size) && is.null(data$linewidth)) {
    data$linewidth <- data$size
  }

  # If data has a 'linewidth' column, validate it
  if (!is.null(data$linewidth)) {
    if (!is.numeric(data$linewidth)) {
      # Try to coerce to numeric
      data$linewidth <- as.numeric(data$linewidth)
      if (all(is.na(data$linewidth))) {
        stop("linewidth must be numeric", call. = FALSE)
      }
    }

    if (any(data$linewidth < 0, na.rm = TRUE)) {
      warning("linewidth values should be non-negative", call. = FALSE)
    }
  }

  return(data)
}

#' Get ggplot function (with gglogger support)
#'
#' @description
#' Centralized function to get the ggplot function with optional gglogger support.
#' This eliminates duplicate code across all plot functions.
#'
#' @return ggplot function (either from ggplot2 or gglogger)
#' @keywords internal
#' @importFrom utils getFromNamespace
get_ggplot <- function() {
  if (getOption("ggforge.gglogger.enabled", FALSE)) {
    getFromNamespace("ggplot", "gglogger")
  } else {
    ggplot2::ggplot
  }
}

#' Normalize legend position (handle waiver)
#'
#' @description
#' Handles waiver objects for legend.position, converting them to actual positions
#' based on whether a grouping variable exists.
#'
#' @param position Legend position (can be waiver or string)
#' @param has_group Logical, whether a grouping variable exists
#' @param default_with_group Default position when group exists
#' @param default_no_group Default position when no group
#' @return Normalized position string
#' @keywords internal
#' @importFrom ggplot2 waiver
normalize_legend_position <- function(
    position,
    has_group,
    default_with_group = "right",
    default_no_group = "none") {
  if (inherits(position, "waiver")) {
    if (has_group) default_with_group else default_no_group
  } else {
    position
  }
}

#' Calculate text justification based on angle
#'
#' @param angle Text angle in degrees
#' @return List with h (hjust) and v (vjust) values
#' @keywords internal
calc_justification <- function(angle) {
  angle <- angle %% 360
  if (angle < 0) angle <- angle + 360

  if (angle < 10) {
    list(h = 0.5, v = 1)
  } else if (angle < 90) {
    list(h = 1, v = 1)
  } else if (angle < 180) {
    list(h = 1, v = 0.5)
  } else if (angle < 270) {
    list(h = 0, v = 0)
  } else if (angle < 315) {
    list(h = 0, v = 0.5)
  } else {
    list(h = 0, v = 1)
  }
}

#' Normalize expansion values (CSS-like padding)
#'
#' @param expand Numeric vector of expansion values
#' @param x_type Type of x-axis ("continuous" or "discrete")
#' @param y_type Type of y-axis ("continuous" or "discrete")
#' @param continuous_default Default for continuous axes
#' @param discrete_default Default for discrete axes
#' @return List with x and y expansion values
#' @keywords internal
#' @importFrom ggplot2 expansion
normalize_expansion <- function(
    expand,
    x_type,
    y_type,
    continuous_default = c(0.05, 0),
    discrete_default = c(0, 0.6)) {
  # Helper to get expansion by type
  expand_by_type <- function(ex, type, both = FALSE) {
    if (type == "continuous") {
      ret <- if (!is.null(ex)) c(ex, 0) else continuous_default
    } else {
      ret <- if (!is.null(ex)) c(0, ex) else discrete_default
    }
    if (both) c(ret, ret) else ret
  }

  if (is.null(expand)) {
    return(list(
      x = expand_by_type(NULL, x_type, both = TRUE),
      y = expand_by_type(NULL, y_type, both = TRUE)
    ))
  }

  # Parse CSS-like notation
  if (is.null(names(expand))) {
    expand <- switch(as.character(length(expand)),
      "1" = c(top = expand, right = expand, bottom = expand, left = expand),
      "2" = c(top = expand[1], right = expand[2], bottom = expand[1], left = expand[2]),
      "3" = c(top = expand[1], right = expand[2], bottom = expand[3], left = expand[2]),
      "4" = c(top = expand[1], right = expand[2], bottom = expand[3], left = expand[4]),
      stop("Invalid expand length", call. = FALSE)
    )
  }

  expand <- as.list(expand)

  # Validate conflicts
  if ("x" %in% names(expand) && any(c("left", "right") %in% names(expand))) {
    stop("Cannot have both 'x' and 'left'/'right' in expand", call. = FALSE)
  }
  if ("y" %in% names(expand) && any(c("top", "bottom") %in% names(expand))) {
    stop("Cannot have both 'y' and 'top'/'bottom' in expand", call. = FALSE)
  }

  # Expand x/y notation
  if ("x" %in% names(expand)) {
    expand$left <- expand$right <- expand$x
    expand$x <- NULL
  }
  if ("y" %in% names(expand)) {
    expand$bottom <- expand$top <- expand$y
    expand$y <- NULL
  }

  list(
    x = c(expand_by_type(expand$left, x_type), expand_by_type(expand$right, x_type)),
    y = c(expand_by_type(expand$bottom, y_type), expand_by_type(expand$top, y_type))
  )
}

#' Add faceting to a plot
#'
#' @param plot ggplot object
#' @param facet_by Column(s) to facet by
#' @param facet_scales Scale type for facets
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param byrow Fill by row
#' @param ... Additional arguments for facet functions
#' @return Faceted plot
#' @keywords internal
#' @importFrom ggplot2 facet_wrap facet_grid vars
#' @importFrom rlang sym
add_facets <- function(
    plot,
    facet_by,
    facet_scales,
    nrow,
    ncol,
    byrow,
    ...) {
  if (is.null(facet_by)) {
    return(plot)
  }

  if (length(facet_by) == 1) {
    plot + facet_wrap(
      facets = facet_by,
      scales = facet_scales,
      nrow = nrow,
      ncol = ncol,
      dir = if (byrow) "h" else "v",
      ...
    )
  } else {
    args <- list(...)
    args$strip.position <- NULL
    args$rows <- vars(!!sym(facet_by[1]))
    args$cols <- vars(!!sym(facet_by[2]))
    args$scales <- facet_scales

    plot + do.call(facet_grid, args)
  }
}

#' Combine multiple plots
#'
#' @param plots List of plot objects
#' @param combine Whether to combine
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param byrow Fill by row
#' @param axes Axis handling
#' @param axis_titles Axis title handling
#' @param guides Guide handling
#' @param design Custom design
#' @return Combined plot or list
#' @keywords internal
#' @importFrom patchwork wrap_plots
combine_plots <- function(
    plots,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = NULL,
    axes = NULL,
    axis_titles = NULL,
    guides = NULL,
    design = NULL) {
  if (!combine) {
    return(plots)
  }

  # Single plot - just return it
  if (length(plots) == 1 && !inherits(plots[[1]], "gTree")) {
    return(plots[[1]])
  }

  # Combine using patchwork
  wrap_plots(
    plots,
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    axes = axes,
    axis_titles = axis_titles,
    guides = guides,
    design = design
  )
}

#' Split data by column
#'
#' @param data Data frame
#' @param split_by Column to split by
#' @return Named list of data frames
#' @keywords internal
split_data <- function(data, split_by) {
  if (is.null(split_by)) {
    return(list("..." = data))
  }

  split(data, data[[split_by]])
}

#' Create background layer for plots
#'
#' @param data Data frame
#' @param x X column
#' @param palette Palette name
#' @param palcolor Custom colors
#' @param alpha Alpha value
#' @param keep_empty Keep empty levels
#' @param facet_by Faceting columns
#' @param direction Background direction
#' @return ggplot layer
#' @keywords internal
#' @importFrom ggplot2 geom_rect aes
#' @importFrom dplyr distinct
#' @importFrom tidyr expand_grid
#' @importFrom rlang sym syms
create_bg_layer <- function(
    data,
    x,
    palette,
    palcolor,
    alpha,
    keep_empty,
    facet_by,
    direction = "vertical") {
  fct <- data[[x]]
  if (!keep_empty) {
    fct <- droplevels(fct)
  }

  bg_color <- get_palette(levels(fct), palette = palette, palcolor = palcolor)

  # Create background data
  bg_data <- data.frame(x = factor(levels(fct), levels = levels(fct)))
  bg_data$x_num <- as.numeric(bg_data$x)
  bg_data$xmin <- ifelse(bg_data$x_num == min(bg_data$x_num), -Inf, bg_data$x_num - 0.5)
  bg_data$xmax <- ifelse(bg_data$x_num == max(bg_data$x_num), Inf, bg_data$x_num + 0.5)
  bg_data$ymin <- -Inf
  bg_data$ymax <- Inf
  bg_data$fill <- bg_color[levels(fct)]

  # Add facet columns if needed
  if (!is.null(facet_by)) {
    unique_facets <- distinct(data, !!!syms(facet_by))
    bg_data <- expand_grid(bg_data, unique_facets)
    for (fb in facet_by) {
      bg_data[[fb]] <- factor(bg_data[[fb]], levels = levels(data[[fb]]))
    }
  }

  # Create appropriate geom based on direction
  if (direction == "vertical") {
    geom_rect(
      data = bg_data,
      aes(
        xmin = !!sym("xmin"), xmax = !!sym("xmax"),
        ymin = !!sym("ymin"), ymax = !!sym("ymax")
      ),
      fill = bg_data$fill,
      alpha = alpha,
      inherit.aes = FALSE
    )
  } else {
    geom_rect(
      data = bg_data,
      aes(
        xmin = !!sym("ymin"), xmax = !!sym("ymax"),
        ymin = !!sym("xmin"), ymax = !!sym("xmax")
      ),
      fill = bg_data$fill,
      alpha = alpha,
      inherit.aes = FALSE
    )
  }
}

#' Blend multiple colors
#'
#' @param colors Vector of colors
#' @param mode Blend mode
#' @return Blended color
#' @keywords internal
#' @importFrom grDevices col2rgb rgb
blend_colors <- function(colors, mode = c("blend", "average", "screen", "multiply")) {
  mode <- match.arg(mode)
  colors <- colors[!is.na(colors)]

  if (length(colors) == 0) {
    return(NA)
  }
  if (length(colors) == 1) {
    return(colors)
  }

  rgb_vals <- col2rgb(colors) / 255

  result <- switch(mode,
    blend = apply(rgb_vals, 1, mean),
    average = apply(rgb_vals, 1, mean),
    screen = 1 - apply(1 - rgb_vals, 1, prod),
    multiply = apply(rgb_vals, 1, prod)
  )

  rgb(result[1], result[2], result[3])
}
