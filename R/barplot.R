#' Bar Plot Atomic
#'
#' @description
#' Creates a single bar plot without splitting. Handles both grouped and ungrouped bar plots.
#'
#' @inheritParams parameters
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual labs scale_x_discrete scale_y_continuous
#' @importFrom ggplot2 element_line element_text position_dodge2 coord_flip coord_cartesian guide_legend
#' @importFrom ggplot2 geom_hline scale_color_manual geom_line geom_point position_dodge waiver
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr group_by summarise n mutate ungroup
#' @importFrom tidyr complete
#' @importFrom rlang sym syms := "%||%"
BarPlotAtomic <- function(
    data, x, x_sep = "_", y = NULL,
    scale_y = FALSE,
    flip = FALSE,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    fill_by_x_if_no_group = TRUE,
    label = NULL,
    label_nudge = 0.02,
    label_fg = "black",
    label_size = 4,
    label_bg = "white",
    label_bg_r = 0.1,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    x_text_angle = 0,
    aspect.ratio = 1,
    add_line = NULL,
    line_color = "red2",
    line_width = 0.6,
    line_type = 2,
    line_name = NULL,
    add_trend = FALSE,
    trend_color = "black",
    trend_linewidth = 1,
    trend_ptsize = 2.5,
    position = "auto",
    position_dodge_preserve = "total",
    y_min = NULL,
    y_max = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
    expand = waiver(),
    width = waiver(),
    facet_by = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
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

  # Handle NULL y (use counts)
  if (is.null(y)) {
    data <- data %>%
      dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
      dplyr::summarise(.y = dplyr::n(), .groups = "drop")
    y <- ".y"
  }

  # Handle label
  if (isTRUE(label)) {
    label <- y
  }
  label <- validate_columns(data, label)

  # Scale y if requested (for grouped plots)
  y_scaled <- NULL
  if (isTRUE(scale_y) && !is.null(group_by)) {
    y_scaled <- paste0(y, "_scaled")
    data <- data %>%
      dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
      dplyr::mutate(!!sym(y_scaled) := !!sym(y) / sum(!!sym(y))) %>%
      dplyr::ungroup()
  }

  # Handle width default
  if (inherits(width, "waiver")) {
    width <- if (is.null(group_by)) 0.9 else 0.8
  }

  # Fill empty factor levels if requested
  if (keep_empty) {
    fill_list <- list(0)
    names(fill_list) <- y
    if (!is.null(y_scaled)) {
      fill_list[[y_scaled]] <- 0
    }

    if (is.null(group_by)) {
      if (is.null(facet_by)) {
        data <- data %>% tidyr::complete(!!sym(x), fill = fill_list)
      } else {
        data <- data %>%
          dplyr::group_by(!!!syms(facet_by)) %>%
          tidyr::complete(!!sym(x), fill = fill_list)
      }
    } else {
      data <- data %>%
        dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
        tidyr::complete(!!sym(group_by), fill = fill_list)
    }
  }

  # Setup expansion
  if (inherits(expand, "waiver")) {
    if (is.null(group_by)) {
      expand <- c(0, 0, 0, 0)
      if (!is.null(label)) {
        if (any(data[[y]] > 0)) {
          expand[1] <- 0.05 + label_nudge * 0.5
        }
        if (any(data[[y]] < 0)) {
          expand[3] <- 0.05 + label_nudge * 0.5
        }
      }
    } else if (position == "stack" || (position == "auto" && length(unique(data[[group_by]])) > 5)) {
      expand <- c(top = 0, bottom = 0)
      if (!is.null(label)) {
        if (any(data[[y]] > 0)) {
          expand["top"] <- 0.05 + label_nudge * 0.5
        }
        if (any(data[[y]] < 0)) {
          expand["bottom"] <- 0.05 + label_nudge * 0.5
        }
      }
    } else if (min(data[[y]], na.rm = TRUE) > 0) {
      expand <- c(bottom = 0)
    } else if (max(data[[y]], na.rm = TRUE) < 0) {
      expand <- c(top = 0)
    } else {
      expand <- NULL
    }
  }
  expand <- normalize_expansion(expand, x_type = "discrete", y_type = "continuous")

  # Calculate text justification
  just <- calc_justification(x_text_angle)

  # Build the plot based on whether group_by is specified
  if (is.null(group_by)) {
    # Single group plot (no grouping variable)
    if (isTRUE(fill_by_x_if_no_group)) {
      p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(x)))
      colors <- get_palette(levels(data[[x]]), palette = palette, palcolor = palcolor)
      guide <- ggplot2::guide_legend(order = 1)
    } else {
      p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = "fill"))
      colors <- get_palette("fill", palette = palette, palcolor = palcolor)
      guide <- "none"
    }

    p <- p + ggplot2::geom_col(alpha = alpha, width = width)

    p <- p +
      ggplot2::scale_fill_manual(name = x, values = colors, guide = guide) +
      ggplot2::labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
      ggplot2::scale_x_discrete(expand = expand$x) +
      ggplot2::scale_y_continuous(expand = expand$y)
  } else {
    # Grouped plot (with grouping variable)
    .y <- if (!is.null(y_scaled)) y_scaled else y
    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(.y), fill = !!sym(group_by)))

    # Add background stripes if requested
    if (isTRUE(add_bg)) {
      p <- p + create_bg_layer(
        data, x, bg_palette, bg_palcolor, bg_alpha,
        keep_empty, facet_by,
        direction = "vertical"
      )
    }

    colors <- get_palette(levels(data[[group_by]]), palette = palette, palcolor = palcolor)

    # Determine position adjustment (auto, dodge, or stack)
    if (position == "auto") {
      position <- if (length(colors) <= 5) {
        ggplot2::position_dodge2(preserve = position_dodge_preserve, width = width)
      } else {
        "stack"
      }
    } else if (position == "dodge") {
      position <- ggplot2::position_dodge2(preserve = position_dodge_preserve, width = width)
    }

    p <- p +
      ggplot2::geom_col(alpha = alpha, position = position, width = width) +
      ggplot2::scale_fill_manual(name = group_name %||% group_by, values = colors, guide = ggplot2::guide_legend(order = 1)) +
      ggplot2::labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
      ggplot2::scale_x_discrete(expand = expand$x) +
      ggplot2::scale_y_continuous(expand = expand$y)

    # Add trend lines connecting bar tops if requested
    if (isTRUE(add_trend)) {
      if (is.null(trend_color)) {
        p <- p +
          ggplot2::geom_line(
            aes(group = !!sym(group_by), color = !!sym(group_by)),
            position = ggplot2::position_dodge(width = 0.9),
            linewidth = trend_linewidth,
            show.legend = FALSE
          ) +
          ggplot2::scale_color_manual(values = colors)
      } else {
        p <- p +
          ggplot2::geom_line(
            aes(group = !!sym(group_by)),
            position = ggplot2::position_dodge(width = 0.9),
            color = trend_color,
            linewidth = trend_linewidth
          )
      }
      p <- p + ggplot2::geom_point(
        position = ggplot2::position_dodge(width = 0.9),
        color = "black",
        fill = "white",
        size = trend_ptsize,
        shape = 21
      )
    }
  }

  # Add value labels if requested
  if (!is.null(label)) {
    label_data <- data
    .y <- if (!is.null(y_scaled)) y_scaled else y

    # Order data for consistent label positioning
    if (is.null(group_by)) {
      label_data <- label_data[order(label_data[[x]]), , drop = FALSE]
    } else {
      label_data <- label_data[order(label_data[[x]], label_data[[group_by]]), , drop = FALSE]
    }

    label_data$.label <- label_data[[label]]
    label_data$.sign <- label_data[[.y]] > 0
    yr <- diff(range(data[[.y]], na.rm = TRUE))

    if (!is.null(group_by) && (identical(position, "stack") || is.character(position) && position == "stack")) {
      # Stacked bar labels (cumulative positioning)
      label_data <- dplyr::mutate(
        label_data,
        !!sym(.y) := ifelse(
          !!sym(".sign"),
          cumsum(rev(!!sym(.y))) + yr * label_nudge,
          cumsum(rev(!!sym(.y))) - yr * label_nudge
        ),
        .label = rev(!!sym(".label")),
        .by = unique(c(x, ".sign", facet_by))
      )

      p <- p + ggrepel::geom_text_repel(
        data = label_data,
        mapping = aes(label = !!sym(".label")),
        color = label_fg,
        size = label_size,
        hjust = if (flip) ifelse(label_data$.sign, 0, 1) else 0.5,
        bg.color = label_bg,
        bg.r = label_bg_r,
        direction = "y",
        force = 0,
        min.segment.length = 0,
        max.overlaps = 100,
        segment.color = "transparent"
      )
    } else {
      # Regular bar labels (individual positioning)
      label_data[[.y]] <- label_data[[.y]] + yr * label_nudge * ifelse(label_data$.sign, 1, -1)

      label_params <- list(
        data = label_data,
        mapping = aes(label = !!sym(".label")),
        color = label_fg,
        size = label_size,
        hjust = if (flip) ifelse(label_data$.sign, 0, 1) else 0.5,
        bg.color = label_bg,
        bg.r = label_bg_r,
        direction = "y",
        force = 0,
        min.segment.length = 0,
        max.overlaps = 100,
        segment.color = "transparent"
      )

      if (!is.null(group_by) && !identical(position, "stack") && !is.character(position)) {
        label_params$position <- position
      }

      p <- p + do.call(ggrepel::geom_text_repel, label_params)
    }
  }

  # Add horizontal reference line if requested
  if (!is.null(add_line)) {
    p <- p +
      ggplot2::geom_hline(
        aes(color = line_name %||% as.character(add_line), yintercept = add_line),
        linetype = line_type,
        linewidth = line_width
      ) +
      ggplot2::scale_color_manual(name = NULL, values = line_color, guide = ggplot2::guide_legend(order = 2))
  }

  # Apply theme and automatic styling
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(
      aspect.ratio = aspect.ratio,
      panel.grid.major = ggplot2::element_line(colour = "grey80", linetype = 2)
    )

  # Apply data-driven styling with smart axis text sizing
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = if (flip) y else x,
    y_var = if (flip) x else y,
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

  # Handle axis flipping and coordinate limits
  facet_free <- !is.null(facet_by) && (
    identical(facet_scales, "free") ||
      (!flip && identical(facet_scales, "free_y")) ||
      (flip && identical(facet_scales, "free_x"))
  )

  if (isTRUE(flip) && !facet_free) {
    p <- p + ggplot2::coord_flip(ylim = c(y_min, y_max))
  } else if (isTRUE(flip)) {
    p <- p + ggplot2::coord_flip()
  } else if (!facet_free) {
    p <- p + ggplot2::coord_cartesian(ylim = c(y_min, y_max))
  }

  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- add_facets(
      p, facet_by, facet_scales,
      facet_nrow, facet_ncol, facet_byrow
    )
  }

  return(p)
}

#' Bar Plot
#'
#' @description
#' Create a bar plot with optional grouping, splitting, and faceting.
#'
#' Bar plots are useful for comparing values across categories. This function
#' supports both simple bar plots and grouped bar plots with stacking or dodging.
#'
#' @inheritParams parameters
#' @param x Column for x-axis (categorical). Will be converted to factor.
#' @param x_sep Separator for concatenating multiple x columns.
#' @param y Column for y-axis (numeric). If NULL, counts will be used.
#' @param scale_y Scale y values to proportions within each x group (only for grouped plots).
#' @param flip Flip x and y axes.
#' @param group_by Column(s) for grouping bars (creates stacked or dodged bars).
#' @param group_by_sep Separator for concatenating multiple group columns.
#' @param group_name Legend title for groups.
#' @param fill_by_x_if_no_group Fill bars by x values when no grouping (default TRUE).
#' @param label Column to use for labels, or TRUE to use y values.
#' @param label_nudge Vertical distance for label positioning.
#' @param label_fg Label text color.
#' @param label_size Label text size.
#' @param label_bg Label background color.
#' @param label_bg_r Label background corner radius.
#' @param add_bg Add background stripes for x categories.
#' @param bg_palette Palette for background stripes.
#' @param bg_palcolor Custom colors for background stripes.
#' @param bg_alpha Transparency for background stripes.
#' @param add_line Add horizontal reference line at specified y value.
#' @param line_color Color of reference line.
#' @param line_width Width of reference line.
#' @param line_type Line type (1=solid, 2=dashed, etc.).
#' @param line_name Legend label for reference line.
#' @param add_trend Add trend line connecting bar tops.
#' @param trend_color Color of trend line (NULL uses group colors).
#' @param trend_linewidth Width of trend line.
#' @param trend_ptsize Size of trend line points.
#' @param position Position adjustment: "auto", "stack", "dodge", or "fill".
#' @param position_dodge_preserve Preserve "total" or "single" width when dodging.
#' @param y_min Minimum y-axis value.
#' @param y_max Maximum y-axis value.
#' @param expand Plot area expansion (CSS-like: top, right, bottom, left).
#' @param width Bar width.
#'
#' @return A ggplot object, list of plots, or combined plots
#' @export
#' @examples
#' \donttest{
#' # Simple bar plot (counts)
#' data <- data.frame(
#'   category = c("A", "B", "C", "D", "A", "B")
#' )
#' BarPlot(data, x = "category")
#'
#' # Bar plot with values
#' data <- data.frame(
#'   category = c("A", "B", "C", "D"),
#'   value = c(10, 15, 8, 12)
#' )
#' BarPlot(data, x = "category", y = "value")
#'
#' # Grouped bar plot (dodged)
#' data <- data.frame(
#'   category = rep(c("A", "B", "C"), 2),
#'   value = c(10, 15, 8, 12, 18, 14),
#'   group = rep(c("G1", "G2"), each = 3)
#' )
#' BarPlot(data, x = "category", y = "value", group_by = "group")
#'
#' # Stacked bar plot
#' BarPlot(data, x = "category", y = "value", group_by = "group", position = "stack")
#'
#' # With labels and customization
#' BarPlot(
#'   data,
#'   x = "category", y = "value", group_by = "group",
#'   label = TRUE, palette = "Set2", flip = TRUE
#' )
#'
#' # With splits (multiple plots)
#' data$experiment <- rep(c("Exp1", "Exp2"), 3)
#' BarPlot(data, x = "category", y = "value", split_by = "experiment")
#'
#' # With faceting (single plot, multiple panels)
#' BarPlot(data, x = "category", y = "value", facet_by = "experiment")
#' }
BarPlot <- function(
    data, x, x_sep = "_", y = NULL,
    scale_y = FALSE,
    flip = FALSE,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    fill_by_x_if_no_group = TRUE,
    split_by = NULL,
    split_by_sep = "_",
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    label = NULL,
    label_nudge = 0.02,
    label_fg = "black",
    label_size = 4,
    label_bg = "white",
    label_bg_r = 0.1,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_line = NULL,
    line_color = "red2",
    line_width = 0.6,
    line_type = 2,
    line_name = NULL,
    add_trend = FALSE,
    trend_color = "black",
    trend_linewidth = 1,
    trend_ptsize = 2.5,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    x_text_angle = 0,
    aspect.ratio = 1,
    y_min = NULL,
    y_max = NULL,
    position = "auto",
    position_dodge_preserve = "total",
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
    expand = waiver(),
    width = waiver(),
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
    atomic_fn = BarPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Split Bar Plot Atomic (Waterfall Plot)
#'
#' @description
#' Creates a split bar plot showing positive and negative values on opposite sides.
#' Also known as a waterfall plot or diverging bar plot.
#'
#' @inheritParams parameters
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_vline geom_col geom_text scale_fill_manual labs
#' @importFrom ggplot2 scale_y_discrete scale_alpha_continuous guide_none guide_legend
#' @importFrom ggplot2 coord_flip coord_cartesian element_blank element_text
#' @importFrom stringr str_wrap
#' @importFrom forcats fct_relabel
#' @importFrom dplyr .data
#' @importFrom rlang sym "%||%"
SplitBarPlotAtomic <- function(
    data, x, y, y_sep = "_",
    flip = FALSE,
    alpha_by = NULL,
    alpha_reverse = FALSE,
    alpha_name = NULL,
    order_y = list("+" = c("x_desc", "alpha_desc"), "-" = c("x_desc", "alpha_asc")),
    bar_height = 0.9,
    lineheight = 0.5,
    max_charwidth = 80,
    fill_by = NULL,
    fill_by_sep = "_",
    fill_name = NULL,
    direction_pos_name = "positive",
    direction_neg_name = "negative",
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    facet_by = NULL,
    facet_scales = "free_y",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    x_min = NULL,
    x_max = NULL,
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
  x <- validate_columns(data, x)
  y <- validate_columns(
    data, y,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = y_sep
  )
  fill_by <- validate_columns(
    data, fill_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = fill_by_sep
  )
  alpha_by <- validate_columns(data, alpha_by)

  # Wrap long text labels for better display
  data[[y]] <- forcats::fct_relabel(data[[y]], stringr::str_wrap, width = max_charwidth)

  # Create direction column based on sign of x values
  data$.direction <- ifelse(data[[x]] > 0, direction_pos_name, direction_neg_name)
  data$.direction <- factor(data$.direction, levels = c(direction_pos_name, direction_neg_name))

  # Default fill by direction (positive vs negative)
  fill_by <- fill_by %||% ".direction"

  # Handle alpha transparency variable
  if (is.null(alpha_by)) {
    data$.alpha <- 1
    alpha_by <- ".alpha"
    alpha_guide <- ggplot2::guide_none()
  } else {
    alpha_guide <- ggplot2::guide_legend(order = 2)
  }

  # Order y-axis categories if requested
  if (!is.null(order_y)) {
    # Validate order_y structure
    if (!is.list(order_y)) {
      order_y <- list("*" = order_y)
    }
    if (length(order_y) != 1 && length(order_y) != 2) {
      stop("'order_y' must be a list of length 1 or 2.", call. = FALSE)
    }
    if (length(order_y) == 1 && names(order_y) != "*") {
      stop("The name of the 'order_y' list must be '*' when it has only one element.", call. = FALSE)
    }
    if (length(order_y) == 2 && !all(c("+", "-") %in% names(order_y))) {
      stop("The names of the 'order_y' list must be '+' and '-' when it has two elements.", call. = FALSE)
    }

    # Validate ordering methods
    for (o in order_y) {
      sapply(o, match.arg, c("x_asc", "x_desc", "alpha_asc", "alpha_desc"))
    }

    # Helper function to order data frame by specified directions
    order_df <- function(df, dirs) {
      order_list <- list()
      for (dir in dirs) {
        if (dir == "x_asc") {
          order_list[[dir]] <- df[[x]]
        } else if (dir == "x_desc") {
          order_list[[dir]] <- -df[[x]]
        } else if (dir == "alpha_asc") {
          order_list[[dir]] <- df[[alpha_by]]
        } else if (dir == "alpha_desc") {
          order_list[[dir]] <- -df[[alpha_by]]
        }
      }
      df[do.call(order, order_list), , drop = FALSE]
    }

    if (length(order_y) == 1) {
      # Order all data together
      data <- order_df(data, order_y[[1]])
    } else {
      # Order positive and negative sides separately
      data_pos <- order_df(data[data$.direction == direction_pos_name, ], order_y[["+"]])
      data_neg <- order_df(data[data$.direction == direction_neg_name, ], order_y[["-"]])
      data <- rbind(data_pos, data_neg)
      rm(data_pos, data_neg)
    }
    data[[y]] <- factor(data[[y]], levels = rev(unique(data[[y]])))
  }

  # Set x-axis limits (symmetric around zero)
  x_min <- x_min %||% -max(abs(data[[x]]))
  x_max <- x_max %||% max(abs(data[[x]]))

  # Build diverging bar plot
  p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y))) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_col(
      aes(fill = !!sym(fill_by), alpha = !!sym(alpha_by)),
      color = "black",
      width = bar_height
    ) +
    ggplot2::scale_fill_manual(
      name = fill_name %||% fill_by,
      values = get_palette(levels(data[[fill_by]]), palette = palette, palcolor = palcolor),
      guide = ggplot2::guide_legend(order = 1)
    ) +
    ggplot2::scale_alpha_continuous(
      name = alpha_name %||% alpha_by,
      range = if (alpha_reverse) c(1, 0.1) else c(0.1, 1),
      guide = alpha_guide
    )

  # Add category labels at center (replace axis text)
  if (isTRUE(flip)) {
    p <- p +
      ggplot2::geom_text(
        aes(
          x = 0, y = !!sym(y),
          label = ifelse(.data[[x]] > 0, gsub("(\\n|$)", " \\1", !!sym(y)), gsub("(^|\\n)", "\\1 ", !!sym(y))),
          hjust = ifelse(.data[[x]] > 0, 1, 0)
        ),
        color = "black",
        lineheight = lineheight,
        angle = 90
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
  } else {
    p <- p +
      ggplot2::geom_text(
        aes(
          x = 0, y = !!sym(y),
          label = ifelse(.data[[x]] > 0, gsub("(\\n|$)", " \\1", !!sym(y)), gsub("(^|\\n)", "\\1 ", !!sym(y))),
          hjust = ifelse(.data[[x]] > 0, 1, 0)
        ),
        color = "black",
        lineheight = lineheight
      ) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  }

  # Add labels and scales
  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
    ggplot2::scale_y_discrete(drop = !keep_empty)

  # Apply theme and automatic styling
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(aspect.ratio = aspect.ratio)

  # Apply data-driven styling with smart axis text sizing
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = if (flip) y else x,
    y_var = if (flip) x else y,
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Add coordinate system with limits
  if (isTRUE(flip)) {
    p <- p + ggplot2::coord_flip(xlim = c(x_min, x_max))
  } else {
    p <- p + ggplot2::coord_cartesian(xlim = c(x_min, x_max))
  }

  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- add_facets(
      p, facet_by, facet_scales,
      facet_nrow, facet_ncol, facet_byrow
    )
  }

  return(p)
}

#' Split Bar Plot (Waterfall Plot)
#'
#' @description
#' Create a split bar plot showing positive and negative values on opposite
#' sides of a central axis. Also known as a waterfall plot or diverging bar plot.
#'
#' This is useful for showing data that has both positive and negative values,
#' such as survey responses (agree/disagree), changes (increase/decrease), or
#' any data with bidirectional nature.
#'
#' @inheritParams parameters
#' @param x Column for x-axis values (numeric, positive and negative).
#' @param y Column(s) for y-axis categories. Will be converted to factor.
#' @param y_sep Separator for concatenating multiple y columns.
#' @param flip Flip x and y axes.
#' @param alpha_by Column to use for transparency (alpha values).
#' @param alpha_reverse Reverse the alpha scale.
#' @param alpha_name Legend title for alpha.
#' @param order_y Ordering specification for y-axis.
#'   List with "+" and "-" keys for positive/negative ordering, or "*" for overall.
#'   Values can be "x_asc", "x_desc", "alpha_asc", "alpha_desc".
#' @param bar_height Height of bars (0-1).
#' @param lineheight Line height for wrapped text labels.
#' @param max_charwidth Maximum characters before wrapping y-axis labels.
#' @param fill_by Column for fill colors (default: direction).
#' @param fill_by_sep Separator for concatenating multiple fill columns.
#' @param fill_name Legend title for fill.
#' @param direction_pos_name Label for positive direction.
#' @param direction_neg_name Label for negative direction.
#' @param x_min Minimum x-axis value.
#' @param x_max Maximum x-axis value.
#'
#' @return A ggplot object, list of plots, or combined plots
#' @export
#' @examples
#' \donttest{
#' # Basic split bar plot (diverging bars)
#' data <- data.frame(
#'   term = c("Item A", "Item B", "Item C", "Item D"),
#'   value = c(-10, 20, -15, 25),
#'   importance = c(1, 3, 2, 4)
#' )
#' SplitBarPlot(data, x = "value", y = "term")
#'
#' # With alpha mapping to show importance
#' SplitBarPlot(data, x = "value", y = "term", alpha_by = "importance")
#'
#' # With custom fill groups
#' data$category <- c("Type A", "Type A", "Type B", "Type B")
#' SplitBarPlot(data, x = "value", y = "term", fill_by = "category")
#'
#' # Custom ordering (by value descending for positive, ascending for negative)
#' SplitBarPlot(
#'   data,
#'   x = "value", y = "term",
#'   order_y = list("+" = "x_desc", "-" = "x_asc")
#' )
#'
#' # Survey-style diverging bars (e.g., agree/disagree)
#' survey_data <- data.frame(
#'   question = rep(c(
#'     "Q1: Service quality", "Q2: Value for money",
#'     "Q3: Overall satisfaction"
#'   ), 2),
#'   response = c(45, 60, 55, -20, -15, -10),
#'   group = rep(c("Positive", "Negative"), each = 3)
#' )
#' SplitBarPlot(
#'   survey_data,
#'   x = "response", y = "question",
#'   direction_pos_name = "Agree",
#'   direction_neg_name = "Disagree",
#'   palette = "RdBu"
#' )
#' }
SplitBarPlot <- function(
    data, x, y, y_sep = "_",
    flip = FALSE,
    split_by = NULL,
    split_by_sep = "_",
    alpha_by = NULL,
    alpha_reverse = FALSE,
    alpha_name = NULL,
    order_y = list("+" = c("x_desc", "alpha_desc"), "-" = c("x_desc", "alpha_asc")),
    bar_height = 0.9,
    lineheight = 0.5,
    max_charwidth = 80,
    fill_by = NULL,
    fill_by_sep = "_",
    fill_name = NULL,
    direction_pos_name = "positive",
    direction_neg_name = "negative",
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    facet_by = NULL,
    facet_scales = "free_y",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    x_min = NULL,
    x_max = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
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
    atomic_fn = SplitBarPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' @rdname SplitBarPlot
#' @export
WaterfallPlot <- SplitBarPlot
