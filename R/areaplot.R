#' Area Plot
#'
#' @description
#' A plot showing how one or more groups' numeric values change over the
#' progression of another variable.
#'
#' @inheritParams parameters
#' @param x A character string of the column name to plot on the x-axis.
#'   A character/factor column is expected.
#' @param x_sep A character string to concatenate the columns in `x`, if multiple columns are provided.
#' @param y A character string of the column name to plot on the y-axis.
#'   A numeric column is expected. If NULL, the count of the x-axis column will be used.
#' @param group_by A character vector of column names to fill the area plot by.
#'   If NULL, the plot will be filled by the first color of the palette.
#'   If multiple columns are provided, the columns will be concatenated with
#'   `group_by_sep` and used as the fill column.
#' @param group_by_sep A character string to separate the columns in `group_by`.
#' @param group_name A character string to name the legend of fill.
#' @param scale_y A logical value to scale the y-axis by the total number in each x-axis group.
#' @param x_text_angle Angle for x-axis text
#' @param keep_empty Whether to keep empty factor levels
#'
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' data <- data.frame(
#'   x = rep(c("A", "B", "C", "D"), 2),
#'   y = c(1, 3, 6, 4, 2, 5, 7, 8),
#'   group = rep(c("F1", "F2"), each = 4),
#'   split = rep(c("X", "Y"), 4)
#' )
#' AreaPlot(data, x = "x", y = "y", group_by = "group")
#' AreaPlot(data,
#'   x = "x", y = "y", group_by = "group",
#'   scale_y = TRUE
#' )
#' AreaPlot(data, x = "x", y = "y", split_by = "group")
#' AreaPlot(data, x = "x", y = "y", split_by = "group", palette = c(F1 = "Blues", F2 = "Reds"))
#' AreaPlot(data,
#'   x = "x", y = "y", group_by = "group", split_by = "split",
#'   legend.direction = c(X = "horizontal", Y = "vertical"),
#'   legend.position = c(X = "top", Y = "right")
#' )
AreaPlot <- function(
    data, x, y = NULL,
    x_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    scale_y = FALSE,
    split_by = NULL,
    split_by_sep = "_",
    facet_by = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
    seed = 8525,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    axes = NULL,
    axis_titles = NULL,
    guides = NULL,
    design = NULL,
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

  # Validate split_by column
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
    atomic_fn = AreaPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Area Plot Atomic
#'
#' @description
#' Creates a single area plot without splitting
#'
#' @inheritParams AreaPlot
#' @keywords internal
#' @importFrom ggplot2 aes geom_area scale_fill_manual labs theme element_line element_text
#' @importFrom ggplot2 scale_x_discrete scale_y_continuous position_stack waiver
#' @importFrom rlang sym syms := "%||%"
AreaPlotAtomic <- function(
    data, x, y = NULL,
    x_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    scale_y = FALSE,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
    ...) {
  # Validate and process columns
  x <- validate_columns(
    data, x,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = x_sep
  )
  y <- validate_columns(data, y)
  group_by <- validate_columns(
    data, group_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = group_by_sep
  )

  # Get ggplot (support gglogger)
  ggplot <- get_ggplot()

  # Handle NULL y (use counts)
  if (is.null(y)) {
    y <- ".count"
    data <- data %>%
      dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
      dplyr::summarise(.count = dplyr::n(), .groups = "drop")
  }

  # Scale y if requested
  if (isTRUE(scale_y)) {
    data <- data %>%
      dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
      dplyr::mutate(!!sym(y) := !!sym(y) / sum(!!sym(y))) %>%
      dplyr::ungroup()
  }

  # Handle group_by
  if (is.null(group_by)) {
    data$.fill <- factor("")
    group_by <- ".fill"
  } else {
    data[[group_by]] <- droplevels(data[[group_by]])

    # Fill missing combinations
    fill_values <- list(0)
    names(fill_values) <- y
    data <- data %>%
      dplyr::group_by(!!!syms(unique(c(group_by, facet_by)))) %>%
      tidyr::complete(!!sym(x), fill = fill_values) %>%
      dplyr::ungroup()
  }

  # Normalize legend position
  legend.position <- normalize_legend_position(
    legend.position,
    has_group = !is.null(group_by) && group_by != ".fill"
  )

  # Get x levels
  xs <- if (isTRUE(keep_empty)) {
    levels(data[[x]])
  } else {
    levels(droplevels(data[[x]]))
  }

  # Calculate text justification
  just <- calc_justification(x_text_angle)

  # Build plot
  p <- ggplot(data, ggplot2::aes(x = as.numeric(!!sym(x)), y = !!sym(y), fill = !!sym(group_by))) +
    ggplot2::geom_area(
      alpha = alpha,
      color = "grey50",
      position = ggplot2::position_stack(vjust = 0.5)
    ) +
    ggplot2::scale_x_discrete(
      drop = !keep_empty,
      expand = c(0, 0),
      breaks = xs,
      limits = xs
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      labels = if (isFALSE(scale_y)) scales::number else scales::percent
    ) +
    ggplot2::scale_fill_manual(
      name = group_name %||% group_by,
      values = get_palette(
        levels(data[[group_by]]),
        palette = palette,
        palcolor = palcolor
      )
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% x,
      y = ylab %||% y
    )

  # Apply theme
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
    x_var = x,
    y_var = y,
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

  # Add faceting
  if (!is.null(facet_by)) {
    p <- add_facets(
      p, facet_by, facet_scales,
      facet_nrow, facet_ncol, facet_byrow
    )
  }

  return(p)
}
