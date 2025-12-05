#' Trend Plot (Atomic)
#'
#' @description
#' Creates a single trend plot without data splitting.
#' A trend plot is like an area plot but with bars showing the trend.
#'
#' @inheritParams parameters
#' @param x Column name for x-axis (categories). Will be converted to factor.
#' @param y Column name for y-axis (values). If NULL, counts will be used.
#' @param x_sep Separator for concatenating multiple x columns.
#' @param group_by Column(s) to group/stack by.
#' @param group_by_sep Separator for concatenating multiple group_by columns.
#' @param group_name Name for the group legend.
#' @param scale_y Whether to scale y-axis to proportions (0-1).
#' @param x_text_angle Angle for x-axis text.
#'
#' @return A ggplot object
#' @keywords internal
#' @importFrom rlang sym syms := "%||%"
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise n mutate ungroup
#' @importFrom tidyr complete
#' @importFrom ggplot2 ggplot aes geom_area geom_col scale_x_discrete
#' @importFrom ggplot2 scale_y_continuous scale_fill_manual labs theme
#' @importFrom ggplot2 element_line element_text position_stack waiver
#' @importFrom scales number percent
TrendPlotAtomic <- function(
    data, x, y = NULL,
    x_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    scale_y = FALSE,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL,
    xlab = NULL, ylab = NULL,
    ...) {
  # Get ggplot function (with gglogger support)
  ggplot <- get_ggplot()

  # Validate and process columns
  x <- validate_columns(
    data, x,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = x_sep
  )
  data[[x]] <- droplevels(data[[x]])

  y <- validate_columns(data, y)

  group_by <- validate_columns(
    data, group_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = group_by_sep
  )

  # Handle NULL y (use counts)
  if (is.null(y)) {
    y <- ".count"
    data <- data %>%
      dplyr::group_by(!!!rlang::syms(unique(c(x, group_by, facet_by)))) %>%
      dplyr::summarise(.count = dplyr::n(), .groups = "drop")
  }

  # Convert to proportions if requested (normalize within each x category)
  if (isTRUE(scale_y)) {
    data <- data %>%
      dplyr::group_by(!!!rlang::syms(unique(c(x, facet_by)))) %>%
      dplyr::mutate(!!rlang::sym(y) := !!rlang::sym(y) / sum(!!rlang::sym(y)))
  }

  # Handle NULL group_by by creating a dummy group for consistent plotting logic
  if (is.null(group_by)) {
    data$.fill <- factor("")
    group_by <- ".fill"
  } else {
    data[[group_by]] <- droplevels(data[[group_by]])

    # Complete missing combinations with zero values to prevent gaps in stacked areas
    fill_values <- list(0)
    names(fill_values) <- y
    data <- data %>%
      dplyr::group_by(!!!rlang::syms(unique(c(group_by, facet_by)))) %>%
      tidyr::complete(!!rlang::sym(x), fill = fill_values) %>%
      dplyr::ungroup()
  }

  # Normalize legend position based on whether grouping exists
  legend.position <- normalize_legend_position(
    legend.position,
    has_group = !is.null(group_by) && group_by != ".fill"
  )

  # Store x levels for dimension calculations
  xs <- levels(data[[x]])

  # Duplicate each row to create wider area effect (bars extend ±0.2 from center)
  nr <- nrow(data)
  dat_area <- data[rep(seq_len(nr), each = 2), , drop = FALSE]
  dat_area[[x]] <- as.numeric(dat_area[[x]])
  dat_area[seq(1, nr * 2, 2), x] <- dat_area[seq(1, nr * 2, 2), x] - 0.2
  dat_area[seq(2, nr * 2, 2), x] <- dat_area[seq(2, nr * 2, 2), x] + 0.2

  # Use centered stacking for proper visual alignment
  position <- ggplot2::position_stack(vjust = 0.5)

  # Calculate text alignment based on rotation angle
  just <- calc_justification(x_text_angle)

  # Build plot
  p <- ggplot(data, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), fill = !!rlang::sym(group_by))) +
    ggplot2::geom_area(
      data = dat_area,
      mapping = ggplot2::aes(x = !!rlang::sym(x), fill = !!rlang::sym(group_by)),
      alpha = alpha / 2,
      color = "grey50",
      position = position
    ) +
    ggplot2::geom_col(
      ggplot2::aes(fill = !!rlang::sym(group_by)),
      width = 0.4,
      color = "black",
      alpha = alpha,
      position = position
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
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
      x = xlab %||% "",
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

  # Apply data-driven styling with custom x-axis text angle
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

  # Calculate dynamic plot dimensions based on number of categories
  height <- if (length(xs) < 10) 4.5 else 6.5
  width <- 0.5 + length(xs) * if (length(xs) < 10) 0.8 else 0.25

  # Adjust dimensions to accommodate legend placement
  if (legend.position %in% c("right", "left")) {
    width <- width + 1
  } else if (legend.direction == "horizontal") {
    height <- height + 1
  } else {
    width <- width + 2
  }

  # Store dimensions as plot attributes for downstream use
  attr(p, "height") <- height
  attr(p, "width") <- width

  # Add faceting if specified
  if (!is.null(facet_by)) {
    p <- add_facets(
      p, facet_by, facet_scales,
      facet_nrow, facet_ncol, facet_byrow
    )
  }

  return(p)
}


#' Trend Plot
#'
#' @description
#' Creates trend plots that combine area and bar visualizations to show trends.
#' Like an area plot but with gaps between the bars.
#' Supports splitting by groups, faceting, and custom color palettes.
#'
#' @seealso \code{\link{AreaPlot}}
#' @inheritParams parameters
#' @inheritParams TrendPlotAtomic
#'
#' @return A ggplot object, patchwork object (if combine=TRUE), or list of plots
#' @export
#'
#' @examples
#' # Create sample data
#' data <- data.frame(
#'   x = rep(c("A", "B", "C", "D"), 2),
#'   y = c(1, 3, 6, 4, 2, 5, 7, 8),
#'   group = rep(c("F1", "F2"), each = 4)
#' )
#'
#' # Basic trend plot
#' TrendPlot(data, x = "x", y = "y", group_by = "group")
#'
#' # With scaled y-axis
#' TrendPlot(data, x = "x", y = "y", group_by = "group", scale_y = TRUE)
#'
#' # Split by group
#' TrendPlot(data, x = "x", y = "y", split_by = "group")
#'
#' # Custom palettes per split
#' TrendPlot(data,
#'   x = "x", y = "y", split_by = "group",
#'   palette = list(F1 = "Set1", F2 = "Paired")
#' )
TrendPlot <- function(
    data, x, y = NULL,
    x_sep = "_",
    split_by = NULL, split_by_sep = "_",
    group_by = NULL, group_by_sep = "_",
    group_name = NULL,
    scale_y = FALSE,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = ggplot2::waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL,
    xlab = NULL, ylab = NULL,
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

  # Validate split_by
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
    atomic_fn = TrendPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}
