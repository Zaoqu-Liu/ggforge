#' Line Plot
#'
#' @description
#' Visualizing the change of a numeric value over the progression of a categorical variable.
#' Supports both single lines and grouped lines with extensive customization options.
#'
#' @inheritParams parameters
#' @param x Column for x-axis (categorical)
#' @param y Column for y-axis (numeric). If NULL, counts will be used
#' @param x_sep Separator for concatenating multiple x columns
#' @param group_by Column to group lines by. If NULL, creates a single line
#' @param group_by_sep Separator for concatenating multiple group_by columns
#' @param group_name Legend name for groups
#' @param fill_point_by_x_if_no_group Logical. If TRUE and no group_by, color points by x values
#' @param color_line_by_x_if_no_group Logical. If TRUE and no group_by, color lines by x values
#' @param add_bg Logical. Add background stripes
#' @param bg_palette Palette for background
#' @param bg_palcolor Custom colors for background
#' @param bg_alpha Alpha value for background
#' @param add_errorbars Logical. Add error bars
#' @param errorbar_width Width of error bars
#' @param errorbar_alpha Alpha value for error bars
#' @param errorbar_color Color for error bars. Use "line" to match line colors
#' @param errorbar_linewidth Line width for error bars
#' @param errorbar_min Column containing lower error bounds
#' @param errorbar_max Column containing upper error bounds
#' @param errorbar_sd Column containing standard deviation. Used if min/max not provided
#' @param highlight Rows to highlight. Can be numeric index, rownames, or expression string
#' @param highlight_size Size of highlighted points
#' @param highlight_color Color for highlighted points
#' @param highlight_alpha Alpha for highlighted points
#' @param pt_alpha Alpha value for points
#' @param pt_size Size of points
#' @param line_type Line type
#' @param line_width Line width
#' @param line_alpha Alpha value for lines
#' @param add_hline Y-intercept for horizontal line(s). Can be numeric or named list for groups
#' @param hline_type Line type for horizontal line
#' @param hline_width Line width for horizontal line
#' @param hline_color Color for horizontal line. Use TRUE to match group colors
#' @param hline_alpha Alpha for horizontal line
#' @param x_text_angle Angle for x-axis text
#' @return A ggplot object or combined plots
#' @export
#' @examples
#' # Basic line plot
#' data <- data.frame(
#'   time = rep(c("T1", "T2", "T3", "T4"), 2),
#'   value = c(10, 15, 13, 20, 8, 12, 11, 18),
#'   group = rep(c("Control", "Treatment"), each = 4)
#' )
#' LinePlot(data, x = "time", y = "value")
#'
#' # Grouped line plot
#' LinePlot(data, x = "time", y = "value", group_by = "group")
#'
#' # With background stripes
#' LinePlot(data, x = "time", y = "value", group_by = "group", add_bg = TRUE)
#'
#' # With error bars
#' data$sd <- c(2, 2.5, 2, 3, 1.5, 2, 1.8, 2.5)
#' LinePlot(
#'   data,
#'   x = "time",
#'   y = "value",
#'   group_by = "group",
#'   add_errorbars = TRUE,
#'   errorbar_sd = "sd"
#' )
#'
#' # With horizontal reference line
#' LinePlot(
#'   data,
#'   x = "time",
#'   y = "value",
#'   group_by = "group",
#'   add_hline = 15,
#'   hline_type = "dashed"
#' )
#'
#' # Split by another variable
#' data$batch <- rep(c("Batch1", "Batch2"), each = 4)
#' LinePlot(data, x = "time", y = "value", group_by = "group", split_by = "batch")
#'
#' # Faceted plot
#' LinePlot(data, x = "time", y = "value", group_by = "group", facet_by = "batch")
LinePlot <- function(
    data, x, y = NULL,
    x_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    fill_point_by_x_if_no_group = TRUE,
    color_line_by_x_if_no_group = TRUE,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_errorbars = FALSE,
    errorbar_width = 0.1,
    errorbar_alpha = 1,
    errorbar_color = "grey30",
    errorbar_linewidth = 0.75,
    errorbar_min = NULL,
    errorbar_max = NULL,
    errorbar_sd = NULL,
    highlight = NULL,
    highlight_size = NULL,
    highlight_color = "red2",
    highlight_alpha = 0.8,
    pt_alpha = 1,
    pt_size = 5,
    line_type = "solid",
    line_width = 1,
    line_alpha = 0.8,
    add_hline = FALSE,
    hline_type = "solid",
    hline_width = 0.5,
    hline_color = "black",
    hline_alpha = 1,
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

  # Set highlight_size default
  if (is.null(highlight_size)) {
    highlight_size <- pt_size - 0.75
  }

  # Collect all parameters for passing to atomic function
  params <- as.list(environment())
  params$data <- NULL # Remove data from params

  # Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = LinePlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Line Plot Atomic
#'
#' @description
#' Creates a single line plot without splitting. Handles both single and grouped lines.
#'
#' @inheritParams parameters
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_errorbar geom_hline
#' @importFrom ggplot2 scale_color_manual scale_fill_manual scale_x_discrete
#' @importFrom ggplot2 labs theme element_text waiver
#' @importFrom dplyr group_by summarise n ungroup filter
#' @importFrom tidyr complete
#' @importFrom rlang sym syms := "%||%" parse_expr
LinePlotAtomic <- function(
    data, x, y = NULL,
    x_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    fill_point_by_x_if_no_group = TRUE,
    color_line_by_x_if_no_group = TRUE,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_errorbars = FALSE,
    errorbar_width = 0.1,
    errorbar_alpha = 1,
    errorbar_color = "grey30",
    errorbar_linewidth = 0.75,
    errorbar_min = NULL,
    errorbar_max = NULL,
    errorbar_sd = NULL,
    highlight = NULL,
    highlight_size = 5,
    highlight_color = "red2",
    highlight_alpha = 0.8,
    pt_alpha = 1,
    pt_size = 5,
    line_type = "solid",
    line_width = 1,
    line_alpha = 0.8,
    add_hline = FALSE,
    hline_type = "solid",
    hline_width = 0.5,
    hline_color = "black",
    hline_alpha = 1,
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
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
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
    data <- data %>%
      dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
      dplyr::summarise(.y = dplyr::n(), .groups = "drop")
    y <- ".y"
  }

  # Process error bars
  if (isTRUE(add_errorbars)) {
    if (is.null(errorbar_sd) && (is.null(errorbar_min) || is.null(errorbar_max))) {
      stop("If 'errorbar_min' and 'errorbar_max' are not provided, 'errorbar_sd' must be provided.", call. = FALSE)
    }
    if (is.null(errorbar_min) || is.null(errorbar_max)) {
      data$errorbar_min <- data[[y]] - data[[errorbar_sd]]
      data$errorbar_max <- data[[y]] + data[[errorbar_sd]]
      errorbar_min <- "errorbar_min"
      errorbar_max <- "errorbar_max"
    }
  }

  # Fill empty levels if requested
  if (keep_empty) {
    fill_list <- list(0)
    names(fill_list) <- y
    if (is.null(group_by)) {
      if (is.null(facet_by)) {
        data <- data %>% tidyr::complete(!!sym(x), fill = fill_list)
      } else {
        data <- data %>%
          dplyr::group_by(!!!syms(facet_by)) %>%
          tidyr::complete(!!sym(x), fill = fill_list) %>%
          dplyr::ungroup()
      }
    } else {
      data <- data %>%
        dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
        tidyr::complete(!!sym(group_by), fill = fill_list) %>%
        dplyr::ungroup()
    }
  }

  # Process highlight
  hidata <- NULL
  if (!is.null(highlight)) {
    if (isTRUE(highlight)) {
      hidata <- data
    } else if (is.numeric(highlight)) {
      hidata <- data[highlight, , drop = FALSE]
    } else if (is.character(highlight) && length(highlight) == 1) {
      hidata <- dplyr::filter(data, !!parse_expr(highlight))
    } else if (!is.null(rownames(data))) {
      hidata <- data[highlight, , drop = FALSE]
    } else {
      stop("No row names in the data, please provide a vector of indexes to highlight.", call. = FALSE)
    }
  }

  # Determine whether this is a grouped or single plot
  is_grouped <- !is.null(group_by)

  # Normalize legend position
  legend.position <- normalize_legend_position(
    legend.position,
    has_group = is_grouped || color_line_by_x_if_no_group || fill_point_by_x_if_no_group
  )

  # Initialize plot
  p <- ggplot(data, ggplot2::aes(x = !!sym(x), y = !!sym(y)))

  # Add background if requested
  if (isTRUE(add_bg)) {
    p <- p + create_bg_layer(
      data, x, bg_palette, bg_palcolor, bg_alpha, keep_empty, facet_by
    )
  }

  # Get colors
  if (is_grouped) {
    colors <- get_palette(
      levels(data[[group_by]]),
      palette = palette,
      palcolor = palcolor
    )
  } else {
    colors <- get_palette(
      levels(data[[x]]),
      palette = palette,
      palcolor = palcolor
    )
  }

  # Add horizontal line if requested
  if (!is.null(add_hline) && !isFALSE(add_hline)) {
    if (is_grouped && isTRUE(hline_color)) {
      # Use group colors for each group
      if (!is.list(add_hline)) {
        add_hline <- stats::setNames(
          as.list(rep(add_hline, length.out = length(levels(data[[group_by]])))),
          levels(data[[group_by]])[1:length(add_hline)]
        )
      }
      add_hline <- add_hline[intersect(levels(data[[group_by]]), names(add_hline))]
      hline_color <- colors[names(add_hline)]
      add_hline <- unlist(add_hline, use.names = FALSE)
    }

    p <- p + ggplot2::geom_hline(
      yintercept = add_hline,
      linetype = hline_type,
      linewidth = hline_width,
      color = hline_color,
      alpha = hline_alpha
    )
  }

  # Add lines
  if (is_grouped) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(color = !!sym(group_by), group = !!sym(group_by)),
        alpha = line_alpha,
        linetype = line_type,
        linewidth = line_width
      ) +
      ggplot2::scale_color_manual(
        name = group_name %||% group_by,
        values = colors,
        guide = "legend",
        drop = !keep_empty
      )
  } else {
    # Single line or colored by x
    if (color_line_by_x_if_no_group) {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(color = !!sym(x), group = 1),
          alpha = line_alpha,
          linetype = line_type,
          linewidth = line_width
        ) +
        ggplot2::scale_color_manual(
          name = x,
          values = colors,
          guide = "legend",
          drop = !keep_empty
        )
    } else {
      p <- p + ggplot2::geom_line(
        ggplot2::aes(group = 1),
        color = colors[[1]],
        alpha = line_alpha,
        linetype = line_type,
        linewidth = line_width
      )
    }
  }

  # Add error bars
  if (isTRUE(add_errorbars)) {
    if (is_grouped && errorbar_color == "line") {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = !!sym(errorbar_min),
          ymax = !!sym(errorbar_max),
          color = !!sym(group_by)
        ),
        alpha = errorbar_alpha,
        width = errorbar_width,
        linewidth = errorbar_linewidth
      )
    } else if (!is_grouped && errorbar_color == "line" && color_line_by_x_if_no_group) {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = !!sym(errorbar_min),
          ymax = !!sym(errorbar_max),
          color = !!sym(x)
        ),
        alpha = errorbar_alpha,
        width = errorbar_width,
        linewidth = errorbar_linewidth
      )
    } else if (!is_grouped && errorbar_color == "line") {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = !!sym(errorbar_min), ymax = !!sym(errorbar_max)),
        color = colors[[1]],
        alpha = errorbar_alpha,
        width = errorbar_width,
        linewidth = errorbar_linewidth
      )
    } else {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = !!sym(errorbar_min), ymax = !!sym(errorbar_max)),
        color = errorbar_color,
        alpha = errorbar_alpha,
        width = errorbar_width,
        linewidth = errorbar_linewidth
      )
    }
  }

  # Add points
  if (is_grouped) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(fill = !!sym(group_by)),
        color = "grey20",
        alpha = pt_alpha,
        size = pt_size,
        shape = 21
      ) +
      ggplot2::scale_fill_manual(
        name = group_name %||% group_by,
        values = colors,
        guide = "legend",
        drop = !keep_empty
      )
  } else {
    # Single group or colored by x
    if (fill_point_by_x_if_no_group) {
      p <- p +
        ggplot2::geom_point(
          ggplot2::aes(fill = !!sym(x)),
          color = "grey20",
          alpha = pt_alpha,
          size = pt_size,
          shape = 21
        ) +
        ggplot2::scale_fill_manual(
          name = x,
          values = colors,
          guide = "legend",
          drop = !keep_empty
        )
    } else {
      p <- p + ggplot2::geom_point(
        fill = colors[[1]],
        color = "grey20",
        alpha = pt_alpha,
        size = pt_size,
        shape = 21
      )
    }
  }

  # Add highlighted points
  if (!is.null(hidata)) {
    p <- p + ggplot2::geom_point(
      data = hidata,
      fill = highlight_color,
      color = "transparent",
      size = highlight_size,
      shape = 21,
      alpha = highlight_alpha
    )
  }

  # Calculate text justification
  just <- calc_justification(x_text_angle)

  # Apply labels
  p <- p +
    ggplot2::scale_x_discrete(drop = !keep_empty) +
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

  # Apply data-driven styling with x-axis angle override
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
      p,
      facet_by = facet_by,
      facet_scales = facet_scales,
      nrow = facet_nrow,
      ncol = facet_ncol,
      byrow = facet_byrow
    )
  }

  return(p)
}
