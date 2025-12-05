#' Ring Plot (Atomic)
#'
#' @description
#' Creates a single ring plot without data splitting. A ring plot is like a
#' pie chart but with multiple concentric rings.
#'
#' @inheritParams parameters
#' @param x Column name for the rings of the plot. If NULL, creates a single ring.
#' @param y Column name for the values. If NULL, counts will be used.
#' @param group_by Column name for how each ring is divided.
#' @param group_by_sep Separator for concatenating multiple group_by columns.
#' @param group_name Name for the group_by variable in the legend.
#' @param label Whether to show labels on the rings. Default is NULL (automatic).
#' @param clockwise Whether to draw the ring plot clockwise (default: TRUE).
#' @param keep_empty Keep empty factor levels.
#'
#' @return A ggplot object
#' @keywords internal
#' @importFrom rlang sym syms := "%||%"
#' @importFrom dplyr group_by summarise n mutate ungroup
#' @importFrom tidyr complete
#' @importFrom ggplot2 ggplot aes geom_col coord_polar scale_fill_manual scale_x_discrete
#' @importFrom ggplot2 element_blank guide_legend labs theme geom_label
RingPlotAtomic <- function(
    data, x = NULL, y = NULL, group_by = NULL,
    group_by_sep = "_", group_name = NULL,
    label = NULL, clockwise = TRUE,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "free_y",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    keep_empty = FALSE,
    ...) {
  # Get ggplot function (support gglogger)
  ggplot <- get_ggplot()

  # Get base size for text scaling
  base_size <- theme_args$base_size %||% 12
  text_size_scale <- base_size / 12

  # Handle NULL x (create a default ring)
  if (is.null(x)) {
    x <- ".x"
    data$.x <- 1
  }

  # Validate and process columns
  x <- validate_columns(data, x, force_factor = TRUE)
  group_by <- validate_columns(
    data, group_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = group_by_sep
  )
  facet_by <- validate_columns(
    data, facet_by,
    force_factor = TRUE,
    allow_multi = TRUE
  )

  # If y is NULL, compute counts for each group
  if (is.null(y)) {
    data <- data %>%
      dplyr::group_by(!!!syms(base::unique(c(x, group_by, facet_by)))) %>%
      dplyr::summarise(.y = dplyr::n(), .groups = "drop")
    y <- ".y"
  } else {
    y <- validate_columns(data, y)
  }

  # Normalize y values so each ring sums to 1 (for proportional display)
  data <- data %>%
    dplyr::group_by(!!!syms(base::unique(c(x, facet_by)))) %>%
    dplyr::mutate(!!sym(y) := !!sym(y) / base::sum(!!sym(y))) %>%
    dplyr::ungroup()

  # Determine rings (reverse order for natural inside-out display)
  rings <- base::rev(base::levels(data[[x]]))

  # Auto-determine label setting (show ring labels only for multiple rings)
  if (base::length(rings) == 1 && is.null(label)) {
    label <- FALSE
  } else if (base::length(rings) > 1 && is.null(label)) {
    label <- TRUE
  }

  # Handle empty levels
  if (isFALSE(keep_empty)) {
    data[[x]] <- base::droplevels(data[[x]])
  } else {
    # Fill y with 0 for empty group_by combinations to preserve structure
    fill_list <- base::list(0)
    base::names(fill_list) <- y
    data <- data %>%
      dplyr::group_by(!!!syms(base::unique(c(x, facet_by)))) %>%
      tidyr::complete(!!sym(group_by), fill = fill_list) %>%
      dplyr::ungroup()
  }

  # Handle clockwise/counter-clockwise direction
  if (isTRUE(clockwise)) {
    data[[group_by]] <- base::factor(data[[group_by]], levels = base::rev(base::levels(data[[group_by]])))
    colors <- get_palette(
      base::rev(base::levels(data[[group_by]])),
      palette = palette,
      palcolor = palcolor
    )
  } else {
    colors <- get_palette(
      base::levels(data[[group_by]]),
      palette = palette,
      palcolor = palcolor
    )
  }

  # Order data by group_by for consistent stacking
  data <- data[base::order(data[[group_by]]), , drop = FALSE]

  # Build plot
  p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(group_by))) +
    ggplot2::geom_col(width = 0.9, color = "white", alpha = alpha) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::scale_fill_manual(
      name = group_name %||% group_by,
      values = colors,
      guide = ggplot2::guide_legend(reverse = clockwise)
    ) +
    ggplot2::scale_x_discrete(limits = c(" ", rings)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% "",
      y = ylab %||% ""
    )

  # Apply base theme
  p <- p + do.call(theme, theme_args)

  # Apply ring-specific styling (override standard styling for polar plot)
  p <- p + ggplot2::theme(
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction,
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank()
  )

  # Add labels if requested
  if (isTRUE(label)) {
    p <- p + ggplot2::geom_label(
      aes(label = !!sym(x), x = !!sym(x), y = 0),
      inherit.aes = FALSE,
      color = "grey20",
      size = text_size_scale * 3
    )
  }

  # Calculate plot dimensions
  height <- 4.5
  width <- 4.5
  if (!identical(legend.position, "none")) {
    if (legend.position %in% c("right", "left")) {
      width <- width + 1
    } else if (legend.direction == "horizontal") {
      height <- height + 1
    } else {
      width <- width + 2
    }
  }

  # Set dimensions as attributes
  attr(p, "height") <- height
  attr(p, "width") <- width

  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- add_facets(
      p, facet_by, facet_scales,
      facet_nrow, facet_ncol, facet_byrow
    )
  }

  return(p)
}


#' Ring Plot
#'
#' @description
#' Creates ring plots to visualize hierarchical proportional data. A ring plot
#' is similar to a pie chart but can display multiple concentric rings, making
#' it useful for showing nested categorical data.
#'
#' @param data A data frame
#' @param x Column name for the rings. If NULL, creates a single ring. Will be converted to factor.
#' @param y Column name for values. If NULL, counts will be used.
#' @param group_by Column name(s) for how each ring is divided.
#'   Multiple columns will be concatenated with group_by_sep.
#' @param group_by_sep Separator for concatenating multiple group_by columns.
#' @param group_name Name for the group_by variable in the legend.
#' @param label Whether to show ring labels. NULL means auto (FALSE for 1 ring, TRUE for multiple).
#' @param clockwise Whether to draw clockwise (default: TRUE).
#' @param split_by Column(s) to split the data by, creating separate plots.
#'   Multiple columns will be concatenated with split_by_sep.
#' @param split_by_sep Separator for concatenating multiple split_by columns.
#' @param facet_by Column(s) to facet by (max 2 columns).
#' @param facet_scales Scale type for facets: "fixed", "free", "free_x", or "free_y".
#' @param facet_ncol Number of columns for faceting.
#' @param facet_nrow Number of rows for faceting.
#' @param facet_byrow Whether to fill facets by row.
#' @param theme Theme name or function. Default is "theme_ggforge".
#' @param theme_args List of arguments to pass to the theme function.
#' @param palette Color palette name. Default is "Paired".
#' @param palcolor Custom colors (overrides palette).
#' @param alpha Transparency level (0-1). Default is 1.
#' @param aspect.ratio Aspect ratio of the plot. Default is 1.
#' @param legend.position Legend position ("none", "left", "right", "bottom", "top").
#' @param legend.direction Legend direction ("horizontal" or "vertical").
#' @param title Plot title. Can be a string or function.
#' @param subtitle Plot subtitle.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param keep_empty Keep empty factor levels.
#' @param combine Whether to combine plots when split_by is used.
#' @param nrow Number of rows when combining plots.
#' @param ncol Number of columns when combining plots.
#' @param byrow Whether to arrange plots by row when combining.
#' @param seed Random seed for reproducibility. Default is 8525.
#' @param axes Axis handling when combining plots.
#' @param axis_titles Axis title handling when combining plots.
#' @param guides Guide handling when combining plots.
#' @param design Custom design for combining plots.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot object, patchwork object (if combine=TRUE), or list of plots
#' @export
#'
#' @examples
#' # Basic ring plot with single ring
#' RingPlot(datasets::iris, group_by = "Species")
#'
#' # Multiple rings
#' data <- data.frame(
#'   x = c("A", "B", "C", "A", "B", "C"),
#'   y = c(1, 2, 3, 4, 5, 6),
#'   group = c("a", "a", "a", "b", "b", "b")
#' )
#' RingPlot(data, x = "x", y = "y", group_by = "group")
#'
#' # With faceting
#' RingPlot(datasets::mtcars, x = "cyl", group_by = "carb", facet_by = "vs")
#'
#' # With splitting and custom palettes
#' RingPlot(datasets::mtcars,
#'   x = "cyl", group_by = "carb", split_by = "vs",
#'   palette = c("0" = "Set1", "1" = "Paired")
#' )
RingPlot <- function(
    data, x = NULL, y = NULL, group_by = NULL,
    group_by_sep = "_", group_name = NULL,
    label = NULL, clockwise = TRUE,
    split_by = NULL, split_by_sep = "_",
    facet_by = NULL, facet_scales = "free_y",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Paired", palcolor = NULL,
    alpha = 1, aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL,
    xlab = NULL, ylab = NULL,
    keep_empty = FALSE,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
    ...) {
  # Validate common arguments
  validate_common_args(
    seed = seed,
    facet_by = facet_by,
    split_by = split_by,
    theme = theme,
    palette = palette,
    alpha = alpha,
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Process theme
  theme <- process_theme(theme)

  # Validate columns
  if (!is.null(x)) {
    x <- validate_columns(data, x, force_factor = TRUE)
  }
  y <- validate_columns(data, y)
  group_by <- validate_columns(
    data, group_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = group_by_sep
  )
  split_by <- validate_columns(
    data, split_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = split_by_sep
  )

  # Collect all parameters for passing to atomic function
  params <- as.list(environment())
  params$data <- NULL # Remove data from params

  # Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = RingPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}
