#' Plot Builder Base Class
#'
#' @description
#' Abstract base system for building consistent plot functions with
#' split/facet/combine support
#'
#' @name plot-builder
#' @keywords internal
NULL

#' Build a plot with standard workflow
#'
#' @description
#' This is the core function that handles the standard plot workflow:
#' 1. Validate parameters
#' 2. Split data if requested
#' 3. Build atomic plots
#' 4. Combine or facet plots
#'
#' @param data Data frame
#' @param atomic_fn Function to create atomic plot
#' @param params List of parameters
#' @param split_by Column to split by
#' @param facet_by Column to facet by
#' @param combine Whether to combine plots
#' @param ... Additional arguments
#' @return Plot or list of plots
#' @keywords internal
build_plot <- function(
    data,
    atomic_fn,
    params,
    split_by = NULL,
    facet_by = NULL,
    combine = TRUE,
    ...) {
  # Validate columns
  if (!is.null(split_by)) {
    split_by <- validate_columns(
      data, split_by,
      force_factor = TRUE,
      allow_multi = TRUE,
      concat_multi = TRUE,
      concat_sep = params$split_by_sep %||% "_"
    )
  }

  # Split data if requested
  if (!is.null(split_by)) {
    data_list <- split_data(data, split_by)
    split_names <- names(data_list)

    # Normalize palette and palcolor for splits
    params$palette <- check_palette(params$palette, split_names)
    params$palcolor <- check_palcolor(params$palcolor, split_names)
    params$legend.position <- check_legend_param(
      params$legend.position, split_names, "legend.position"
    )
    params$legend.direction <- check_legend_param(
      params$legend.direction, split_names, "legend.direction"
    )

    # Build plots for each split
    plots <- lapply(split_names, function(name) {
      split_params <- params
      split_params$palette <- params$palette[[name]]
      split_params$palcolor <- params$palcolor[[name]]
      split_params$legend.position <- params$legend.position[[name]]
      split_params$legend.direction <- params$legend.direction[[name]]
      split_params$title <- format_title(
        params$title, name, paste0(split_by, ": ", name)
      )

      do.call(atomic_fn, c(list(data = data_list[[name]]), split_params))
    })
    names(plots) <- split_names

    # Combine plots
    return(combine_plots(
      plots,
      combine = combine,
      nrow = params$nrow,
      ncol = params$ncol,
      byrow = params$byrow,
      axes = params$axes,
      axis_titles = params$axis_titles,
      guides = params$guides,
      design = params$design
    ))
  } else {
    # Single plot
    params$title <- format_title(params$title, NULL, NULL)
    return(do.call(atomic_fn, c(list(data = data), params)))
  }
}

#' Format title with dynamic content
#'
#' @param title Title (string or function)
#' @param split_name Name of current split
#' @param default_title Default title
#' @return Formatted title
#' @keywords internal
format_title <- function(title, split_name, default_title) {
  if (is.null(title)) {
    return(default_title)
  }

  if (is.function(title)) {
    return(title(default_title))
  }

  return(title)
}

#' Check and normalize legend parameter
#'
#' @param legend_param Legend position or direction
#' @param split_names Names of splits
#' @param param_name Parameter name for error messages
#' @return Normalized legend parameter
#' @keywords internal
#' @importFrom ggplot2 waiver
check_legend_param <- function(legend_param, split_names, param_name) {
  # Handle waiver first before converting to list
  if (inherits(legend_param, "waiver")) {
    legend_param <- list(legend_param)
  } else {
    legend_param <- as.list(legend_param)
  }

  # Check if first element is waiver (in case it was already a list)
  if (length(legend_param) > 0 && inherits(legend_param[[1]], "waiver")) {
    legend_param <- list(legend_param[[1]])
  }

  # Replicate if needed
  if (length(legend_param) == 1 && length(split_names) > 1) {
    legend_param <- rep(legend_param, length(split_names))
  }

  if (length(legend_param) < length(split_names)) {
    stop(
      sprintf(
        "%s length (%d) less than split_by values (%d)",
        param_name, length(legend_param), length(split_names)
      ),
      call. = FALSE
    )
  }

  # Set names
  if (is.null(names(legend_param))) {
    names(legend_param)[seq_along(split_names)] <- split_names
  } else {
    missing <- setdiff(split_names, names(legend_param))
    if (length(missing) > 0) {
      stop(
        sprintf("Missing %s for split_by values: %s", param_name, paste(missing, collapse = ", ")),
        call. = FALSE
      )
    }
  }

  return(legend_param)
}

#' Standard atomic plot wrapper
#'
#' @description
#' Wraps atomic plot functions with consistent pre/post processing
#'
#' @param data Data frame
#' @param plot_fn Function to create the plot
#' @param params Parameters
#' @return ggplot object
#' @keywords internal
wrap_atomic_plot <- function(data, plot_fn, params) {
  # Apply theme processing
  params$theme <- process_theme(params$theme %||% "theme_ggforge")

  # Build plot
  plot <- do.call(plot_fn, c(list(data = data), params))

  # Add faceting if requested
  if (!is.null(params$facet_by)) {
    plot <- add_facets(
      plot,
      facet_by = params$facet_by,
      facet_scales = params$facet_scales,
      nrow = params$facet_nrow,
      ncol = params$facet_ncol,
      byrow = params$facet_byrow
    )
  }

  return(plot)
}

#' Template for creating standard plot functions
#'
#' @description
#' This is a template showing the standard structure for plot functions.
#' All plot functions should follow this pattern:
#'
#' 1. A main exported function that:
#'    - Validates common arguments
#'    - Processes theme
#'    - Handles splits via build_plot()
#'
#' 2. An atomic function that:
#'    - Creates the actual ggplot
#'    - Takes simple, validated inputs
#'    - Returns a ggplot object
#'
#' @name plot-template
#' @keywords internal
NULL

# Example structure (not actual code):
#
# ExamplePlot <- function(
#     data, x, y,
#     split_by = NULL, split_by_sep = "_",
#     theme = "theme_ggforge", theme_args = list(),
#     palette = "Paired", palcolor = NULL,
#     ...) {
#
#   # Validate common arguments
#   validate_common_args(...)
#
#   # Process theme
#   theme <- process_theme(theme)
#
#   # Get default parameters
#   params <- get_default_params("example")
#   params <- merge_params(as.list(environment()), params)
#
#   # Build plot using standard workflow
#   build_plot(
#     data = data,
#     atomic_fn = ExamplePlotAtomic,
#     params = params,
#     split_by = split_by,
#     ...
#   )
# }
#
# ExamplePlotAtomic <- function(
#     data, x, y,
#     theme = "theme_ggforge", theme_args = list(),
#     palette = "Paired", palcolor = NULL,
#     ...) {
#
#   # Validate columns
#   x <- validate_columns(data, x)
#   y <- validate_columns(data, y)
#
#   # Create plot
#   p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y))) +
#     geom_point() +
#     do.call(theme, theme_args)
#
#   return(p)
# }
