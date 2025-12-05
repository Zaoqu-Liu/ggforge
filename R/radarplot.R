#' Radar Plot / Spider Plot
#'
#' @description
#' Create a radar plot (circular grid) or spider plot (polygonal grid) for
#' visualizing multivariate data across multiple categories.
#'
#' @rdname radarplot
#' @inheritParams parameters
#' @param x Column name for the x-axis/circles. Character/factor column expected.
#' @param x_sep Separator for concatenating multiple x columns.
#' @param group_by Column name(s) for grouping data (the lines). Character/factor column(s) expected.
#' @param group_by_sep Separator for concatenating multiple group_by columns.
#' @param y Column name for the y-axis. Numeric column expected.
#'   If NULL, counts of x-axis column in each group will be used.
#' @param group_name Legend title for groups.
#' @param scale_y How to scale the y-axis. Options: "group", "global", "x", "none".
#'   * "group": Scale to fraction within each group
#'   * "global": Scale to fraction of total
#'   * "x": Scale to fraction within each x-axis group
#'   * "none": Use raw counts/values
#' @param y_min Minimum value of y-axis.
#' @param y_max Maximum value of y-axis.
#' @param y_nbreaks Number of breaks in y-axis.
#' @param polygon Draw polygons instead of circles as panel grid.
#' @param bg_color Background color of the plot.
#' @param bg_alpha Transparency of background color.
#' @param fill Fill polygons with colors.
#' @param linewidth Width of the lines.
#' @param pt_size Size of the points.
#' @param max_charwidth Maximum character width for x labels.
#' @return A ggplot object, wrapped plots, or list of plots
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' # Use counts
#' data <- data.frame(
#'   x = c(rep("A", 2), rep("B", 3), rep("C", 3), rep("D", 4), rep("E", 5)),
#'   group = sample(paste0("G", 1:4), 17, replace = TRUE)
#' )
#' RadarPlot(data, x = "x")
#' RadarPlot(data, x = "x", bg_color = "lightpink")
#' RadarPlot(data, x = "x", scale_y = "none")
#' RadarPlot(data, x = "x", group_by = "group")
#' SpiderPlot(data, x = "x")
#' SpiderPlot(data, x = "x", group_by = "group")
#'
#' # Use y values
#' data <- data.frame(
#'   x = rep(LETTERS[1:5], 2),
#'   y = c(1, 3, 6, 4, 2, 5, 7, 8, 9, 10),
#'   group = rep(c("G1", "G2"), each = 5)
#' )
#' RadarPlot(data, x = "x", y = "y", scale_y = "none", group_by = "group")
#' RadarPlot(data, x = "x", y = "y", facet_by = "group")
#' RadarPlot(data, x = "x", y = "y", split_by = "group")
#' RadarPlot(data,
#'   x = "x", y = "y", split_by = "group",
#'   palette = c(G1 = "Set1", G2 = "Paired")
#' )
RadarPlot <- function(
    data, x, x_sep = "_",
    group_by = NULL, group_by_sep = "_",
    y = NULL, group_name = NULL,
    scale_y = c("group", "global", "x", "none"),
    y_min = 0, y_max = NULL, y_nbreaks = 4,
    bg_color = "grey80", bg_alpha = 0.1,
    fill = TRUE, linewidth = 1, pt_size = 4,
    max_charwidth = 16,
    split_by = NULL, split_by_sep = "_",
    facet_by = NULL, facet_scales = "fixed",
    facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Paired", palcolor = NULL,
    alpha = 0.2, aspect.ratio = 1,
    legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL,
    seed = 8525, combine = TRUE,
    nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = NULL,
    guides = NULL, design = NULL, ...) {
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

  # Collect all parameters for passing to atomic function
  params <- as.list(environment())
  params$data <- NULL # Remove data from params
  params$polygon <- FALSE # RadarPlot uses circular grid

  # Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = RadarPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' @rdname radarplot
#' @export
#' @importFrom ggplot2 waiver
SpiderPlot <- function(
    data, x, x_sep = "_",
    group_by = NULL, group_by_sep = "_",
    y = NULL, group_name = NULL,
    scale_y = c("group", "global", "x", "none"),
    y_min = 0, y_max = NULL, y_nbreaks = 4,
    bg_color = "grey80", bg_alpha = 0.1,
    fill = TRUE, linewidth = 1, pt_size = 4,
    max_charwidth = 16,
    split_by = NULL, split_by_sep = "_",
    facet_by = NULL, facet_scales = "fixed",
    facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Paired", palcolor = NULL,
    alpha = 0.2, aspect.ratio = 1,
    legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL,
    seed = 8525, combine = TRUE,
    nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = NULL,
    guides = NULL, design = NULL, ...) {
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

  # Collect all parameters for passing to atomic function
  params <- as.list(environment())
  params$data <- NULL # Remove data from params
  params$polygon <- TRUE # SpiderPlot uses polygonal grid

  # Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = RadarPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Radar Plot Atomic
#'
#' @description
#' Creates a single radar or spider plot without splitting
#'
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_polygon geom_point geom_text geom_path
#' @importFrom ggplot2 scale_y_continuous scale_x_discrete scale_fill_manual scale_color_manual
#' @importFrom ggplot2 coord_polar labs theme element_blank element_line element_text element_rect
#' @importFrom ggplot2 ggproto CoordPolar waiver margin unit
#' @importFrom rlang sym syms := "%||%"
#' @importFrom dplyr group_by summarise n mutate ungroup select distinct
#' @importFrom tidyr complete expand_grid
#' @importFrom scales number percent label_wrap
RadarPlotAtomic <- function(
    data, x, x_sep = "_",
    group_by = NULL, group_by_sep = "_",
    y = NULL, group_name = NULL,
    scale_y = c("group", "global", "x", "none"),
    y_min = 0, y_max = NULL, y_nbreaks = 4,
    polygon = FALSE,
    bg_color = "grey80", bg_alpha = 0.1,
    fill = TRUE, linewidth = 1, pt_size = 4,
    max_charwidth = 16,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Paired", palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed",
    facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    alpha = 0.2, aspect.ratio = 1,
    legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL, ...) {
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

  facet_by <- validate_columns(
    data, facet_by,
    force_factor = TRUE,
    allow_multi = TRUE
  )

  # Handle NULL y (use counts)
  if (is.null(y)) {
    y <- ".count"
    data <- data %>%
      dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
      dplyr::summarise(.count = dplyr::n(), .groups = "drop")
  }

  # Handle NULL group_by by creating a dummy group for consistent behavior
  if (is.null(group_by)) {
    data$.group <- factor("")
    group_by <- ".group"
  }

  # Normalize legend position
  legend.position <- normalize_legend_position(
    legend.position,
    has_group = !is.null(group_by) && group_by != ".group"
  )

  # Scale y values
  scale_y <- match.arg(scale_y)
  if (scale_y == "group") {
    data <- data %>%
      dplyr::group_by(!!!syms(unique(c(group_by, facet_by)))) %>%
      dplyr::mutate(!!sym(y) := !!sym(y) / sum(!!sym(y))) %>%
      dplyr::ungroup()
  } else if (scale_y == "global") {
    if (!is.null(facet_by)) {
      data <- data %>%
        dplyr::group_by(!!!syms(unique(facet_by))) %>%
        dplyr::mutate(!!sym(y) := !!sym(y) / sum(!!sym(y))) %>%
        dplyr::ungroup()
    } else {
      data <- data %>%
        dplyr::mutate(!!sym(y) := !!sym(y) / sum(!!sym(y)))
    }
  } else if (scale_y == "x") {
    data <- data %>%
      dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
      dplyr::mutate(!!sym(y) := !!sym(y) / sum(!!sym(y))) %>%
      dplyr::ungroup()
  }

  # Fill missing combinations with 0
  complete_fill <- list(0)
  names(complete_fill) <- y
  data <- data %>%
    dplyr::group_by(!!!syms(unique(c(group_by, facet_by)))) %>%
    tidyr::complete(!!sym(x), fill = complete_fill) %>%
    dplyr::ungroup()

  # Create custom radar coordinate system
  coord_radar <- function(theta = "x", start = 0, direction = 1) {
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") "y" else "x"
    ggplot2::ggproto(
      "CoordRadar", ggplot2::CoordPolar,
      theta = theta, r = r, start = start, clip = "off",
      direction = sign(direction),
      is_linear = function(coord) TRUE
    )
  }

  # Calculate y-axis breaks
  y_min <- y_min %||% min(data[[y]])
  y_max <- y_max %||% max(data[[y]])
  breaks <- seq(y_min, y_max, length.out = y_nbreaks)

  # Initialize plot
  p <- ggplot(data, aes(
    x = !!sym(x),
    y = !!sym(y),
    group = !!sym(group_by),
    color = !!sym(group_by)
  ))

  # Add background (circle or polygon)
  x_levels <- levels(data[[x]])
  n_x <- length(x_levels)

  if (polygon) {
    # Polygonal background - connects discrete x-axis points
    bg_df <- data.frame(
      x = factor(c(x_levels, x_levels[1], rev(x_levels), rev(x_levels)[1]), levels = x_levels),
      y = c(rep(y_max, n_x), y_max, rep(y_min, n_x), y_min),
      bg_group = 1
    )

    p <- p + ggplot2::geom_polygon(
      data = bg_df,
      aes(x = !!sym("x"), y = !!sym("y"), group = !!sym("bg_group")),
      fill = bg_color, alpha = bg_alpha, color = NA, inherit.aes = FALSE
    )
  } else {
    # Circular background - smooth interpolated circle
    n_smooth <- 360
    x_positions <- seq(0.5, n_x + 0.5, length.out = n_smooth + 1)

    bg_df <- data.frame(
      x_pos = c(x_positions, rev(x_positions)),
      y = c(rep(y_max, n_smooth + 1), rep(y_min, n_smooth + 1)),
      bg_group = 1
    )

    p <- p + ggplot2::geom_polygon(
      data = bg_df,
      aes(x = !!sym("x_pos"), y = !!sym("y"), group = !!sym("bg_group")),
      fill = bg_color, alpha = bg_alpha, color = NA, inherit.aes = FALSE
    )
  }

  # Add radial grid lines
  if (!is.null(facet_by)) {
    grid_df <- data %>%
      dplyr::select(!!!syms(facet_by)) %>%
      dplyr::distinct()
    grid_df <- tidyr::expand_grid(grid_df, x = levels(data[[x]]))
  } else {
    grid_df <- data.frame(x = levels(data[[x]]))
  }

  # Create two points for each radial line
  n_lines <- nrow(grid_df)
  grid_df <- grid_df[rep(seq_len(n_lines), each = 2), , drop = FALSE]
  grid_df$y <- rep(c(y_min, y_max), times = n_lines)
  grid_df$line_id <- rep(seq_len(n_lines), each = 2)

  p <- p + ggplot2::geom_path(
    data = grid_df,
    aes(x = !!sym("x"), y = !!sym("y"), group = !!sym("line_id")),
    colour = "grey80", linetype = 1, inherit.aes = FALSE
  )

  # Add circular/polygonal grid lines at breaks
  if (isTRUE(polygon)) {
    for (i in seq_along(breaks)) {
      p <- p + ggplot2::geom_polygon(
        y = breaks[i],
        fill = "transparent",
        color = "grey80",
        linetype = 2
      )
    }
  }

  # Add data polygon/line
  if (isTRUE(fill)) {
    p <- p + ggplot2::geom_polygon(
      aes(fill = !!sym(group_by)),
      alpha = alpha,
      linewidth = linewidth
    )
  } else {
    p <- p + ggplot2::geom_polygon(
      alpha = alpha,
      linewidth = linewidth,
      fill = "transparent"
    )
  }

  # Add break labels
  if (!is.null(facet_by)) {
    labels_df <- data %>%
      dplyr::select(!!!syms(facet_by)) %>%
      dplyr::distinct()
    labels_df <- tidyr::expand_grid(labels_df, breaks)
  } else {
    labels_df <- data.frame(breaks = breaks)
  }

  # Calculate panel spacing based on label width
  mc <- min(max(nchar(levels(data[[x]]))), max_charwidth)
  panel.spacing <- theme_args$panel.spacing %||% ggplot2::unit(mc * 2, "points")
  theme_args$panel.spacing <- NULL

  # Add points, labels, scales, and theme
  p <- p +
    ggplot2::geom_point(
      aes(colour = !!sym(group_by), fill = !!sym(group_by)),
      size = pt_size,
      shape = 21
    ) +
    ggplot2::geom_text(
      data = labels_df,
      mapping = aes(
        y = !!sym("breaks"),
        label = if (scale_y == "none") {
          scales::number(!!sym("breaks"))
        } else {
          scales::percent(!!sym("breaks"))
        }
      ),
      x = pi / (length(breaks) - 0.88),
      color = "grey20",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = breaks,
      expand = c(.1, 0, 0, 0)
    ) +
    ggplot2::scale_x_discrete(labels = scales::label_wrap(max_charwidth)) +
    ggplot2::scale_fill_manual(
      name = group_name %||% group_by,
      values = get_palette(
        levels(data[[group_by]]),
        palette = palette,
        palcolor = palcolor
      )
    ) +
    ggplot2::scale_color_manual(
      values = get_palette(
        levels(data[[group_by]]),
        palette = palette,
        palcolor = palcolor
      )
    ) +
    coord_radar(start = -pi / nlevels(data[[x]])) +
    ggplot2::labs(title = title, subtitle = subtitle)

  # Apply theme and automatic styling
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.spacing = panel.spacing
    )

  # Apply data-driven styling for x-axis
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = x,
    y_var = NULL,
    flip = FALSE,
    base_size = base_size,
    legend.position = "none",
    legend.direction = "vertical"
  )

  # Add circular grid lines for non-polygon plots
  if (isFALSE(polygon)) {
    p <- p + ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(
        colour = c(rep("grey80", length(breaks)), NA),
        linetype = 2
      )
    )
  }

  # Set plot dimensions
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
