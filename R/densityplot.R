#' Density Plot / Histogram / Ridge Plot
#'
#' @description
#' Functions for creating density plots, histograms, and ridge plots to visualize distributions
#'
#' @name densityplot
NULL

#' Density Plot Atomic Function
#'
#' @description
#' Internal function to create density or histogram plots without splitting
#'
#' @param data A data frame
#' @param x Column name for values (numeric expected)
#' @param group_by Column to group the data
#' @param group_by_sep Separator for concatenating multiple group_by columns
#' @param group_name Legend title for group_by
#' @param xtrans Transformation for x-axis (e.g., "identity", "log10")
#' @param ytrans Transformation for y-axis
#' @param type Plot type: "density" or "histogram"
#' @param bins Number of bins for histogram
#' @param binwidth Width of bins for histogram
#' @param flip Whether to flip the plot
#' @param add_bars Whether to add data distribution lines at bottom
#' @param bar_height Height of distribution bars (as fraction of max)
#' @param bar_alpha Alpha for distribution bars
#' @param bar_width Width of distribution bars
#' @param position Position adjustment for overlapping groups
#' @param use_trend Use trend line instead of histogram bars
#' @param add_trend Add trend line to histogram
#' @param trend_alpha Alpha for trend line and points
#' @param trend_linewidth Width of trend line
#' @param trend_pt_size Size of trend points
#' @param trend_skip_zero Skip zero counts in trend line
#' @param palette Palette name
#' @param palcolor Custom colors
#' @param alpha Transparency for density/histogram
#' @param theme Theme function
#' @param theme_args Arguments for theme function
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param expand Axis expansion
#' @param facet_by Columns for faceting
#' @param facet_scales Facet scales type
#' @param facet_ncol Number of facet columns
#' @param facet_nrow Number of facet rows
#' @param facet_byrow Fill facets by row
#' @param legend.position Legend position
#' @param legend.direction Legend direction
#' @param ... Additional arguments passed to geoms
#' @return A ggplot object
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_density geom_histogram geom_linerange
#' @importFrom ggplot2 scale_fill_manual scale_color_manual scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 labs theme coord_flip stat_bin waiver
#' @importFrom rlang sym "%||%"
#' @importFrom zoo na.approx
DensityHistoPlotAtomic <- function(
    data, x,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    xtrans = "identity",
    ytrans = "identity",
    type = c("density", "histogram"),
    bins = NULL,
    binwidth = NULL,
    flip = FALSE,
    add_bars = FALSE,
    bar_height = 0.025,
    bar_alpha = 1,
    bar_width = 0.1,
    position = "identity",
    use_trend = FALSE,
    add_trend = FALSE,
    trend_alpha = 1,
    trend_linewidth = 0.8,
    trend_pt_size = 1.5,
    trend_skip_zero = FALSE,
    palette = "Paired",
    palcolor = NULL,
    alpha = 0.5,
    theme = "theme_ggforge",
    theme_args = list(),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    legend.position = "right",
    legend.direction = "vertical",
    ...) {
  # Get ggplot function
  ggplot <- get_ggplot()

  # Match arguments
  type <- match.arg(type)

  # Normalize expansion
  expand <- normalize_expansion(expand, x_type = "continuous", y_type = "continuous")

  # Validate columns
  x <- validate_columns(data, x)
  group_by <- validate_columns(
    data, group_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = group_by_sep
  )

  # Handle NULL group_by for consistent plotting behavior
  if (is.null(group_by)) {
    group_by <- ".group"
    data[[group_by]] <- factor("")
  }

  # Normalize legend position
  legend.position <- normalize_legend_position(
    legend.position,
    has_group = !is.null(group_by) && group_by != ".group"
  )

  # Set default bins for histogram
  if (is.null(bins) && is.null(binwidth) && type == "histogram") {
    bins <- 30
    message("Using `bins = 30`. Pick better value with `binwidth`.")
  }

  # Calculate bar positions if requested
  if (isTRUE(add_bars)) {
    if (type == "density") {
      # Calculate the max density for the y-axis
      max_y <- max(stats::density(data[[x]])$y) * 1.5
    } else {
      # Calculate the max count for the y-axis by bins
      if (is.null(bins) && is.null(binwidth)) {
        s <- seq(min(data[[x]]), max(data[[x]]), length.out = 30)
      } else if (!is.null(bins)) {
        s <- seq(min(data[[x]]), max(data[[x]]), length.out = bins)
      } else {
        s <- seq(min(data[[x]]), max(data[[x]]), by = binwidth)
      }
      max_y <- max(table(cut(data[[x]], s)))
    }
    lnheight <- bar_height * max_y
    # Calculate the ymin ymax for each group to plot the data lines
    data$.ymin <- lnheight * (1 - as.integer(data[[group_by]]))
    data$.ymax <- data$.ymin - lnheight
  }

  # Build plot
  p <- ggplot(data, aes(x = !!sym(x), fill = !!sym(group_by), color = !!sym(group_by))) +
    scale_fill_manual(
      name = group_name %||% group_by,
      values = get_palette(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
    ) +
    scale_color_manual(
      name = group_name %||% group_by,
      values = get_palette(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
    )

  # Add geom based on type
  if (type == "histogram") {
    if (!use_trend) {
      p <- p + geom_histogram(alpha = alpha, bins = bins, binwidth = binwidth, position = position, ...)
    }
    if (use_trend || add_trend) {
      p <- p + stat_bin(
        geom = "point", bins = bins, binwidth = binwidth, alpha = trend_alpha,
        size = trend_pt_size, position = position, ...
      )
      if (trend_skip_zero) {
        if (inherits(ytrans, "transform")) {
          ytrans_obj <- ytrans
        } else if (is.character(ytrans)) {
          ytrans_obj <- getFromNamespace(paste0("transform_", ytrans), "scales")()
        } else if (is.function(ytrans)) {
          ytrans_obj <- ytrans()
        } else {
          stop("ytrans should be a character, a transform object, or a function returning a transform object.")
        }
        p <- p + stat_bin(
          aes(y = after_stat({
            y <- ifelse(!!sym("count") > 0, !!sym("count"), NA)
            y <- ytrans_obj$transform(y)
            y <- split(y, !!sym("..group.."))
            y <- unlist(lapply(y, na.approx, na.rm = FALSE))
            ytrans_obj$inverse(y)
          })),
          bins = bins, binwidth = binwidth,
          geom = "line", position = position, linewidth = trend_linewidth, ...
        )
      } else {
        p <- p + stat_bin(
          aes(y = after_stat(!!sym("count"))),
          bins = bins, binwidth = binwidth,
          geom = "line", position = position, linewidth = trend_linewidth, ...
        )
      }
    }
  } else {
    p <- p + geom_density(alpha = alpha, position = position, ...)
  }

  # Add bars if requested
  if (isTRUE(add_bars)) {
    p <- p + geom_linerange(
      aes(ymin = !!sym(".ymin"), ymax = !!sym(".ymax")),
      alpha = bar_alpha, linewidth = bar_width
    )
  }

  # Add scales and theme
  p <- p +
    scale_x_continuous(expand = expand$x, transform = xtrans) +
    scale_y_continuous(
      expand = expand$y,
      transform = ytrans,
      labels = if (type == "histogram") scales::number else scales::number
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% x,
      y = ylab %||% ifelse(type == "histogram", "Count", "Density")
    )

  # Apply theme and automatic styling
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p + do.call(theme, theme_args)

  # Apply data-driven styling (automatic axis text sizing based on variable types)
  # Note: y-axis shows density/count which is continuous, so we pass ".density"
  # (internal columns starting with "." are treated as continuous)
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = x,
    y_var = ".density",
    flip = flip,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Flip if requested
  if (flip) {
    p <- p + coord_flip()
  }

  # Add faceting
  if (!is.null(facet_by)) {
    p <- add_facets(
      p, facet_by, facet_scales,
      facet_nrow, facet_ncol, facet_byrow
    )
  }

  return(p)
}

#' Density Plot
#'
#' @description
#' Creates density plots to illustrate the distribution of continuous data.
#' Supports grouping, faceting, and splitting into multiple plots.
#'
#' @inheritParams parameters
#' @param x Column name for values (numeric expected)
#' @param group_by Column to group the data
#' @param group_by_sep Separator for concatenating multiple group_by columns
#' @param group_name Legend title for group_by
#' @param xtrans Transformation for x-axis (e.g., "identity", "log10")
#' @param ytrans Transformation for y-axis
#' @param split_by Column to split data into multiple plots
#' @param split_by_sep Separator for concatenating multiple split_by columns
#' @param flip Whether to flip the plot
#' @param position Position adjustment for overlapping groups
#' @param palette Palette name
#' @param palcolor Custom colors
#' @param alpha Transparency for density curves
#' @param theme Theme name or function
#' @param theme_args Arguments passed to theme function
#' @param add_bars Whether to add data distribution lines at bottom
#' @param bar_height Height of distribution bars (as fraction of max)
#' @param bar_alpha Alpha for distribution bars
#' @param bar_width Width of distribution bars
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param expand Axis expansion
#' @param facet_by Columns for faceting
#' @param facet_scales Facet scales type
#' @param facet_ncol Number of facet columns
#' @param facet_nrow Number of facet rows
#' @param facet_byrow Fill facets by row
#' @param legend.position Legend position
#' @param legend.direction Legend direction
#' @param seed Random seed
#' @param combine Whether to combine multiple plots
#' @param nrow Number of rows for combined plots
#' @param ncol Number of columns for combined plots
#' @param byrow Fill combined plots by row
#' @param axes Axis handling for combined plots
#' @param axis_titles Axis title handling for combined plots
#' @param guides Guide handling for combined plots
#' @param design Custom design for combined plots
#' @param ... Additional arguments passed to geom_density
#'
#' @return A ggplot object, combined plot, or list of plots
#' @export
#' @examples
#' set.seed(8525)
#' data <- data.frame(
#'   x = c(rnorm(500, -1), rnorm(500, 1)),
#'   group = rep(c("A", "B"), each = 500),
#'   facet = sample(c("F1", "F2"), 1000, replace = TRUE)
#' )
#'
#' DensityPlot(data, x = "x")
#' DensityPlot(data, x = "x", group_by = "group")
#' DensityPlot(data, x = "x", group_by = "group", facet_by = "facet")
#' DensityPlot(data, x = "x", split_by = "facet", add_bars = TRUE)
DensityPlot <- function(
    data, x,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    xtrans = "identity",
    ytrans = "identity",
    split_by = NULL,
    split_by_sep = "_",
    flip = FALSE,
    position = "identity",
    palette = "Paired",
    palcolor = NULL,
    alpha = 0.5,
    theme = "theme_ggforge",
    theme_args = list(),
    add_bars = FALSE,
    bar_height = 0.025,
    bar_alpha = 1,
    bar_width = 0.1,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL,
    facet_scales = "free_y",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    legend.position = waiver(),
    legend.direction = "vertical",
    seed = 8525,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    axes = NULL,
    axis_titles = axes,
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
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Process theme
  theme <- process_theme(theme)

  # Collect parameters (exclude top-level only params)
  params <- as.list(environment())
  params$data <- NULL
  params$split_by <- NULL
  params$split_by_sep <- NULL
  params$combine <- NULL
  params$nrow <- NULL
  params$ncol <- NULL
  params$byrow <- NULL
  params$seed <- NULL
  params$axes <- NULL
  params$axis_titles <- NULL
  params$guides <- NULL
  params$design <- NULL
  params$type <- "density"

  # Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = DensityHistoPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Histogram
#'
#' @description
#' Creates histograms to illustrate the distribution of continuous data.
#' Supports grouping, faceting, splitting, and trend lines.
#'
#' @inheritParams DensityPlot
#' @param bins Number of bins for histogram
#' @param binwidth Width of bins for histogram
#' @param use_trend Use trend line instead of histogram bars
#' @param add_trend Add trend line to histogram
#' @param trend_alpha Alpha for trend line and points
#' @param trend_linewidth Width of trend line
#' @param trend_pt_size Size of trend points
#' @param trend_skip_zero Skip zero counts in trend line
#'
#' @return A ggplot object, combined plot, or list of plots
#' @export
#' @examples
#' set.seed(8525)
#' data <- data.frame(
#'   x = sample(setdiff(1:100, c(30:36, 50:55, 70:77)), 1000, replace = TRUE),
#'   group = factor(rep(c("A", "B"), each = 500), levels = c("A", "B")),
#'   facet = sample(c("F1", "F2"), 1000, replace = TRUE)
#' )
#'
#' Histogram(data, x = "x")
#' Histogram(data, x = "x", group_by = "group")
#' Histogram(data, x = "x", split_by = "facet", add_bars = TRUE)
#' Histogram(data, x = "x", group_by = "group", add_trend = TRUE)
Histogram <- function(
    data, x,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    xtrans = "identity",
    ytrans = "identity",
    split_by = NULL,
    split_by_sep = "_",
    flip = FALSE,
    bins = NULL,
    binwidth = NULL,
    trend_skip_zero = FALSE,
    add_bars = FALSE,
    bar_height = 0.025,
    bar_alpha = 1,
    bar_width = 0.1,
    position = "identity",
    use_trend = FALSE,
    add_trend = FALSE,
    trend_alpha = 1,
    trend_linewidth = 0.8,
    trend_pt_size = 1.5,
    palette = "Paired",
    palcolor = NULL,
    alpha = 0.5,
    theme = "theme_ggforge",
    theme_args = list(),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL,
    facet_scales = "free_y",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    legend.position = waiver(),
    legend.direction = "vertical",
    seed = 8525,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    axes = NULL,
    axis_titles = axes,
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
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Process theme
  theme <- process_theme(theme)

  # Collect parameters (exclude top-level only params)
  params <- as.list(environment())
  params$data <- NULL
  params$split_by <- NULL
  params$split_by_sep <- NULL
  params$combine <- NULL
  params$nrow <- NULL
  params$ncol <- NULL
  params$byrow <- NULL
  params$seed <- NULL
  params$axes <- NULL
  params$axis_titles <- NULL
  params$guides <- NULL
  params$design <- NULL
  params$type <- "histogram"

  # Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = DensityHistoPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Ridge Plot Atomic Function
#'
#' @description
#' Internal function to create ridge plots without splitting
#'
#' @param data A data frame (long or wide form)
#' @param x Column name for values (numeric expected)
#' @param in_form Data format: "long" or "wide"
#' @param group_by Column(s) to group the data (shown on y-axis)
#' @param group_by_sep Separator for concatenating multiple group_by columns
#' @param group_name Legend title for group_by
#' @param add_vline Add vertical lines (TRUE for mean, numeric vector, or named list)
#' @param vline_type Line type for vertical lines
#' @param vline_color Color for vertical lines (TRUE to match groups)
#' @param vline_width Width of vertical lines
#' @param vline_alpha Alpha for vertical lines
#' @param flip Whether to flip the plot
#' @param alpha Transparency for ridges
#' @param scale Scaling factor for ridges
#' @param theme Theme function
#' @param theme_args Arguments for theme function
#' @param palette Palette name
#' @param palcolor Custom colors
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param x_text_angle Angle for x-axis text
#' @param keep_empty Keep empty groups on y-axis
#' @param reverse Reverse order of groups on y-axis
#' @param facet_by Columns for faceting
#' @param facet_scales Facet scales type
#' @param facet_ncol Number of facet columns
#' @param facet_nrow Number of facet rows
#' @param facet_byrow Fill facets by row
#' @param aspect.ratio Aspect ratio
#' @param legend.position Legend position
#' @param legend.direction Legend direction
#' @param ... Additional arguments passed to geom_density_ridges
#' @return A ggplot object
#' @keywords internal
#' @importFrom ggplot2 ggplot aes scale_fill_manual scale_y_discrete scale_x_continuous
#' @importFrom ggplot2 labs theme element_text element_line coord_flip geom_vline
#' @importFrom rlang sym syms "%||%"
#' @importFrom tidyr pivot_longer
RidgePlotAtomic <- function(
    data,
    x = NULL,
    in_form = c("long", "wide"),
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    add_vline = NULL,
    vline_type = "solid",
    vline_color = TRUE,
    vline_width = 0.5,
    vline_alpha = 1,
    flip = FALSE,
    alpha = 0.8,
    scale = NULL,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    x_text_angle = 90,
    keep_empty = FALSE,
    reverse = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = "none",
    legend.direction = "vertical",
    ...) {
  # Get ggplot function
  ggplot <- get_ggplot()

  # Match arguments
  in_form <- match.arg(in_form)

  # Handle wide form
  if (in_form == "wide") {
    data <- tidyr::pivot_longer(data, cols = group_by, names_to = ".group", values_to = ".x")
    x <- ".x"
    group_by <- ".group"
  }

  # Validate columns
  x <- validate_columns(data, x)
  group_by <- validate_columns(
    data, group_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = group_by_sep
  )

  # Handle NULL group_by
  if (is.null(group_by)) {
    group_by <- ".group"
    data[[group_by]] <- factor(" ")
  }

  # Reverse if requested
  if (isTRUE(reverse)) {
    data[[group_by]] <- factor(data[[group_by]], levels = rev(levels(data[[group_by]])))
  }

  # Get colors
  colors <- get_palette(levels(data[[group_by]]), palette = palette, palcolor = palcolor)

  # Build plot
  p <- ggplot(data, aes(x = !!sym(x), y = !!sym(group_by), fill = !!sym(group_by)))

  # Add ridge density
  if (!is.null(scale)) {
    p <- p + ggridges::geom_density_ridges(alpha = alpha, scale = scale, ...)
  } else {
    p <- p + ggridges::geom_density_ridges(alpha = alpha, ...)
  }

  # Add vertical lines
  if (!is.null(add_vline) && !isFALSE(add_vline)) {
    if (isTRUE(add_vline)) {
      # Calculate the mean of each group
      add_vline <- tapply(data[[x]], data[[group_by]], mean, na.rm = TRUE)
    }
    if (isTRUE(vline_color)) {
      if (!is.list(add_vline)) {
        add_vline <- as.list(add_vline)
        if (reverse) {
          names(add_vline) <- rev(levels(data[[group_by]]))[1:length(add_vline)]
        } else {
          names(add_vline) <- levels(data[[group_by]])[1:length(add_vline)]
        }
      }
      add_vline <- add_vline[intersect(levels(data[[group_by]]), names(add_vline))]
      vline_color <- sapply(
        colors[names(add_vline)],
        function(cl) blend_colors(c(cl, cl, cl), mode = "multiply")
      )
      add_vline <- unlist(add_vline, use.names = FALSE)
    }
    p <- p + geom_vline(
      xintercept = add_vline, linetype = vline_type, linewidth = vline_width,
      color = vline_color, alpha = vline_alpha
    )
  }

  # Add scales and theme
  p <- p +
    scale_fill_manual(values = colors) +
    scale_y_discrete(drop = !keep_empty, expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% x,
      y = ylab %||% group_by
    )

  # Handle flip
  if (flip) {
    just <- calc_justification(x_text_angle)
    p <- p +
      ggplot2::theme(
        axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v),
        axis.ticks.x = element_line(),
        panel.grid.major.x = element_line(color = "grey", linetype = 2)
      ) +
      coord_flip()
  } else {
    p <- p +
      ggplot2::theme(
        axis.text.x = element_text(),
        axis.text.y = element_text(hjust = 1),
        axis.ticks.y = element_line(),
        panel.grid.major.y = element_line(color = "grey", linetype = 2)
      )
  }

  # Apply theme and automatic styling
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(aspect.ratio = aspect.ratio)

  # Apply data-driven styling
  # x is continuous (data), y is discrete (groups)
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = x,
    y_var = group_by,
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
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

#' Ridge Plot
#'
#' @description
#' Creates ridge plots to illustrate the distribution of data across multiple groups.
#' Groups are displayed on the y-axis with overlapping density curves.
#'
#' @param data A data frame (long or wide form)
#' @param x Column name for values (numeric expected)
#' @param in_form Data format: "long" or "wide"
#' @param split_by Column to split data into multiple plots
#' @param split_by_sep Separator for concatenating multiple split_by columns
#' @param group_by Column(s) to group the data (shown on y-axis)
#' @param group_by_sep Separator for concatenating multiple group_by columns
#' @param group_name Legend title for group_by
#' @param scale Scaling factor for ridges (higher = more overlap)
#' @param add_vline Add vertical lines (TRUE for mean, numeric vector, or named list)
#' @param vline_type Line type for vertical lines
#' @param vline_color Color for vertical lines (TRUE to match groups)
#' @param vline_width Width of vertical lines
#' @param vline_alpha Alpha for vertical lines
#' @param flip Whether to flip the plot
#' @param alpha Transparency for ridges
#' @param theme Theme name or function
#' @param theme_args Arguments passed to theme function
#' @param palette Palette name
#' @param palcolor Custom colors
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param x_text_angle Angle for x-axis text
#' @param keep_empty Keep empty groups on y-axis
#' @param reverse Reverse order of groups on y-axis
#' @param facet_by Columns for faceting
#' @param facet_scales Facet scales type
#' @param facet_ncol Number of facet columns
#' @param facet_nrow Number of facet rows
#' @param facet_byrow Fill facets by row
#' @param aspect.ratio Aspect ratio
#' @param legend.position Legend position
#' @param legend.direction Legend direction
#' @param combine Whether to combine multiple plots
#' @param nrow Number of rows for combined plots
#' @param ncol Number of columns for combined plots
#' @param byrow Fill combined plots by row
#' @param seed Random seed
#' @param axes Axis handling for combined plots
#' @param axis_titles Axis title handling for combined plots
#' @param guides Guide handling for combined plots
#' @param design Custom design for combined plots
#' @param ... Additional arguments passed to geom_density_ridges
#'
#' @return A ggplot object, combined plot, or list of plots
#' @export
#' @examples
#' set.seed(8525)
#' data <- data.frame(
#'   x = c(rnorm(250, -1), rnorm(250, 1)),
#'   group = rep(LETTERS[1:5], each = 100)
#' )
#' RidgePlot(data, x = "x") # fallback to density plot
#' RidgePlot(data, x = "x", add_vline = 0, vline_color = "black")
#' RidgePlot(data, x = "x", group_by = "group")
#' RidgePlot(data, x = "x", group_by = "group", reverse = TRUE)
#' RidgePlot(data, x = "x", group_by = "group", add_vline = TRUE, vline_color = TRUE)
#'
#' # wide form
#' data_wide <- data.frame(
#'   A = rnorm(100),
#'   B = rnorm(100),
#'   C = rnorm(100),
#'   D = rnorm(100),
#'   E = rnorm(100),
#'   group = sample(letters[1:4], 100, replace = TRUE)
#' )
#' RidgePlot(data_wide, group_by = LETTERS[1:5], in_form = "wide")
#' RidgePlot(data_wide, group_by = LETTERS[1:5], in_form = "wide", facet_by = "group")
RidgePlot <- function(
    data,
    x = NULL,
    in_form = c("long", "wide"),
    split_by = NULL,
    split_by_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    scale = NULL,
    add_vline = NULL,
    vline_type = "solid",
    vline_color = TRUE,
    vline_width = 0.5,
    vline_alpha = 1,
    flip = FALSE,
    alpha = 0.8,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    x_text_angle = 90,
    keep_empty = FALSE,
    reverse = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = "none",
    legend.direction = "vertical",
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    seed = 8525,
    axes = NULL,
    axis_titles = axes,
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

  # Collect parameters (exclude top-level only params)
  params <- as.list(environment())
  params$data <- NULL
  params$split_by <- NULL
  params$split_by_sep <- NULL
  params$combine <- NULL
  params$nrow <- NULL
  params$ncol <- NULL
  params$byrow <- NULL
  params$seed <- NULL
  params$axes <- NULL
  params$axis_titles <- NULL
  params$guides <- NULL
  params$design <- NULL

  # Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = RidgePlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}
