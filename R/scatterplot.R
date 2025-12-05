#' Scatter Plot (Atomic)
#'
#' @description
#' Creates a single scatter plot without data splitting
#'
#' @inheritParams parameters
#' @param x Column name for x-axis. Must be a numeric column.
#' @param y Column name for y-axis. Must be a numeric column.
#' @param size_by Column name to use for point size, or a numeric value for fixed size.
#' @param size_name Name for the size legend.
#' @param color_by Column name to use for point color. Can be numeric or categorical.
#' @param color_name Name for the color legend.
#' @param color_reverse Whether to reverse the color direction.
#' @param border_color Color for point borders, or TRUE to match fill color.
#' @param highlight Vector of row indices/names or expression string to highlight points.
#' @param highlight_shape Shape for highlighted points.
#' @param highlight_size Size for highlighted points.
#' @param highlight_color Color for highlighted points.
#' @param highlight_alpha Alpha for highlighted points.
#' @param shape Shape of points (default 21 which has fill).
#' @param xtrans Transformation for x-axis (e.g. "log10", "sqrt").
#' @param ytrans Transformation for y-axis (e.g. "log10", "sqrt").
#'
#' @return A ggplot object
#' @keywords internal
#' @importFrom rlang sym syms := "%||%" parse_expr
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_point scale_size_area scale_fill_gradientn
#' @importFrom ggplot2 scale_color_gradientn scale_fill_manual scale_color_manual
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous labs theme element_line
#' @importFrom ggplot2 guide_colorbar guide_legend guide_none guides
ScatterPlotAtomic <- function(
    data, x, y,
    size_by = 2, size_name = NULL,
    color_by = NULL, color_name = NULL, color_reverse = FALSE,
    highlight = NULL, highlight_shape = 16, highlight_size = 3,
    highlight_color = "red", highlight_alpha = 1,
    theme = "theme_ggforge", theme_args = list(),
    alpha = NULL,
    shape = 21,
    border_color = "black",
    xtrans = "identity",
    ytrans = "identity",
    palette = NULL,
    palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL,
    xlab = NULL, ylab = NULL,
    ...) {
  # Get ggplot function (with gglogger support)
  ggplot <- get_ggplot()

  # Validate facet_by
  facet_by <- validate_columns(
    data, facet_by,
    force_factor = TRUE,
    allow_multi = TRUE
  )

  # Validate size_by
  if (!is.numeric(size_by)) {
    size_by <- validate_columns(data, size_by)
  }

  # Validate color_by
  color_by <- validate_columns(data, color_by)

  # Handle NULL color_by: create dummy column for consistent ggplot2 mapping
  color_legend <- TRUE
  if (is.null(color_by)) {
    data$.color_by <- ""
    color_by <- ".color_by"
    color_legend <- FALSE
  } else if (!is.numeric(data[[color_by]])) {
    color_by <- validate_columns(data, color_by, force_factor = TRUE)
  }

  # Set default alpha: filled shapes need transparency to see overlaps
  if (is.null(alpha)) {
    alpha <- if (shape %in% 21:25) 0.65 else 1
  }

  # Set default palette: categorical data uses discrete palette, numeric uses gradient
  if (is.null(palette)) {
    palette <- if (!is.null(color_by) && color_by != ".color_by" && !is.numeric(data[[color_by]])) {
      "Paired"
    } else {
      "Spectral"
    }
  }

  # Handle highlight
  hidata <- NULL
  if (!is.null(highlight)) {
    if (isTRUE(highlight)) {
      hidata <- data
    } else if (is.numeric(highlight)) {
      hidata <- data[highlight, , drop = FALSE]
    } else if (is.character(highlight) && length(highlight) == 1) {
      hidata <- dplyr::filter(data, !!rlang::parse_expr(highlight))
    } else if (is.null(rownames(data))) {
      stop("No row names in the data, please provide a vector of indexes to highlight.", call. = FALSE)
    } else {
      hidata <- data[highlight, , drop = FALSE]
    }
  }

  # Determine if shapes have fill
  shape_has_fill <- shape %in% 21:25
  hishape_has_fill <- highlight_shape %in% 21:25

  # Build base plot
  p <- ggplot(data, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y)))

  # Build mapping for geom_point
  mapping <- list()
  point_layer_args <- list(shape = shape)

  if (shape_has_fill) {
    mapping[[length(mapping) + 1]] <- ggplot2::aes(fill = !!rlang::sym(color_by))
    if (isTRUE(border_color)) {
      mapping[[length(mapping) + 1]] <- ggplot2::aes(color = !!rlang::sym(color_by))
    } else {
      point_layer_args$color <- border_color
    }
  } else {
    mapping[[length(mapping) + 1]] <- ggplot2::aes(color = !!rlang::sym(color_by))
  }

  if (is.numeric(size_by)) {
    point_layer_args$size <- size_by
  } else {
    mapping[[length(mapping) + 1]] <- ggplot2::aes(size = !!rlang::sym(size_by))
  }

  # Combine mappings using ggplot2's internal utility
  modify_list <- utils::getFromNamespace("modify_list", "ggplot2")
  point_layer_args$mapping <- Reduce(modify_list, mapping)
  point_layer <- do.call(ggplot2::geom_point, point_layer_args)

  p <- p + point_layer

  # Add size scale if size_by is a column
  if (!is.numeric(size_by)) {
    p <- p +
      ggplot2::scale_size_area(max_size = 6, n.breaks = 4) +
      ggplot2::guides(size = ggplot2::guide_legend(
        title = size_name %||% size_by,
        override.aes = list(fill = "grey30", shape = 21),
        order = 1
      ))
  }

  # Add color/fill scales (skip if no real color mapping)
  if (shape_has_fill && color_legend) {
    if (is.numeric(data[[color_by]])) {
      # Numeric color with fill
      p <- p + ggplot2::scale_fill_gradientn(
        n.breaks = 5,
        colors = get_palette(
          data[[color_by]],
          palette = palette,
          palcolor = palcolor,
          reverse = color_reverse,
          alpha = alpha
        ),
        na.value = "grey80",
        guide = if (isTRUE(border_color)) {
          ggplot2::guide_none()
        } else {
          ggplot2::guide_colorbar(
            title = color_name %||% color_by,
            frame.colour = "black",
            ticks.colour = "black",
            frame.linewidth = 0.3,
            ticks.linewidth = 0.3,
            title.hjust = 0,
            order = 2
          )
        }
      )
      if (isTRUE(border_color) && color_legend) {
        p <- p + ggplot2::scale_color_gradientn(
          n.breaks = 5,
          colors = get_palette(
            palette = palette,
            palcolor = palcolor,
            reverse = color_reverse
          ),
          na.value = "grey80",
          guide = ggplot2::guide_colorbar(
            title = color_name %||% color_by,
            frame.colour = "black",
            ticks.colour = "black",
            frame.linewidth = 0.3,
            ticks.linewidth = 0.3,
            title.hjust = 0,
            order = 2
          )
        )
      }
    } else {
      # Categorical color with fill
      p <- p + ggplot2::scale_fill_manual(
        values = get_palette(
          levels(data[[color_by]]),
          palette = palette,
          palcolor = palcolor,
          alpha = alpha
        ),
        na.value = "grey80",
        guide = if (isTRUE(border_color)) {
          ggplot2::guide_none()
        } else {
          ggplot2::guide_legend(
            title = color_name %||% color_by,
            override.aes = list(size = 4, alpha = 1),
            order = 3
          )
        }
      )
      if (isTRUE(border_color) && color_legend) {
        p <- p + ggplot2::scale_color_manual(
          values = get_palette(
            levels(data[[color_by]]),
            palette = palette,
            palcolor = palcolor
          ),
          na.value = "black",
          guide = ggplot2::guide_legend(
            title = color_name %||% color_by,
            order = 3
          )
        )
      }
    }
  } else if (color_legend) {
    # Shape has no fill - only add scales if there's a real color mapping
    if (is.numeric(data[[color_by]])) {
      # Numeric color
      p <- p + ggplot2::scale_color_gradientn(
        n.breaks = 5,
        colors = get_palette(
          data[[color_by]],
          palette = palette,
          palcolor = palcolor,
          reverse = color_reverse,
          alpha = alpha
        ),
        na.value = "grey80",
        guide = ggplot2::guide_colorbar(
          title = color_name %||% color_by,
          frame.colour = "black",
          ticks.colour = "black",
          frame.linewidth = 0.3,
          ticks.linewidth = 0.3,
          title.hjust = 0,
          order = 2
        )
      )
    } else {
      # Categorical color
      p <- p + ggplot2::scale_color_manual(
        values = get_palette(
          levels(data[[color_by]]),
          palette = palette,
          palcolor = palcolor,
          alpha = alpha
        ),
        na.value = "grey80",
        guide = ggplot2::guide_legend(
          title = color_name %||% color_by,
          override.aes = list(size = 4, alpha = 1),
          order = 3
        )
      )
    }
  }

  # Add highlight layer
  if (!is.null(hidata)) {
    if (hishape_has_fill) {
      p <- p + ggplot2::geom_point(
        data = hidata,
        shape = highlight_shape,
        fill = highlight_color,
        color = "transparent",
        size = highlight_size,
        alpha = highlight_alpha
      )
    } else {
      p <- p + ggplot2::geom_point(
        data = hidata,
        shape = highlight_shape,
        color = highlight_color,
        size = highlight_size,
        alpha = highlight_alpha
      )
    }
  }

  # Add scales and labels
  p <- p +
    ggplot2::scale_x_continuous(trans = xtrans) +
    ggplot2::scale_y_continuous(trans = ytrans) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% x,
      y = ylab %||% y
    )

  # Apply theme and automatic styling
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(
      aspect.ratio = aspect.ratio,
      panel.grid.major = ggplot2::element_line(colour = "grey80", linetype = 2)
    )

  # Apply data-driven styling
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = x,
    y_var = y,
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Calculate plot dimensions
  height <- 5
  width <- 5
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


#' Scatter Plot
#'
#' @description
#' Creates scatter plots with support for size mapping, color mapping,
#' highlighting specific points, and custom transformations.
#' Supports splitting by groups and faceting.
#'
#' @inheritParams parameters
#' @inheritParams ScatterPlotAtomic
#'
#' @return A ggplot object, patchwork object (if combine=TRUE), or list of plots
#' @export
#'
#' @examples
#' # Create sample data
#' set.seed(8525)
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   size_val = abs(rnorm(100)),
#'   category = sample(c("A", "B", "C"), 100, replace = TRUE),
#'   group = sample(c("Group1", "Group2"), 100, replace = TRUE)
#' )
#'
#' # Basic scatter plot
#' ScatterPlot(data, x = "x", y = "y")
#'
#' # Highlight specific points
#' ScatterPlot(data, x = "x", y = "y", highlight = "x > 1 & y > 0")
#'
#' # Size by numeric column
#' ScatterPlot(data, x = "x", y = "y", size_by = "size_val")
#'
#' # Color by numeric column with continuous gradient
#' ScatterPlot(data, x = "x", y = "y", color_by = "size_val", palette = "RdYlBu")
#'
#' # Color by categorical column
#' ScatterPlot(data, x = "x", y = "y", color_by = "category")
#'
#' # Combine size and color mapping with custom border
#' ScatterPlot(data,
#'   x = "x", y = "y", size_by = "size_val", color_by = "category",
#'   border_color = "black"
#' )
#'
#' # Border color matches fill color (for cohesive look)
#' ScatterPlot(data,
#'   x = "x", y = "y", color_by = "category",
#'   border_color = TRUE
#' )
#'
#' # Use shape without fill (solid points)
#' ScatterPlot(data,
#'   x = "x", y = "y", color_by = "category",
#'   shape = 16, palette = "Set1"
#' )
#'
#' # Split by group to create separate panels
#' ScatterPlot(data,
#'   x = "x", y = "y", color_by = "category",
#'   split_by = "group", combine = TRUE
#' )
#'
#' # Facet by group (alternative to split_by)
#' ScatterPlot(data, x = "x", y = "y", color_by = "category", facet_by = "group")
#'
#' # Log transformation of axes
#' data_pos <- data.frame(
#'   x = 10^rnorm(100, 2, 1),
#'   y = 10^rnorm(100, 3, 0.5)
#' )
#' ScatterPlot(data_pos, x = "x", y = "y", xtrans = "log10", ytrans = "log10")
ScatterPlot <- function(
    data, x, y,
    size_by = 2, size_name = NULL,
    color_by = NULL, color_name = NULL, color_reverse = FALSE,
    split_by = NULL, split_by_sep = "_",
    shape = 21,
    alpha = NULL,
    border_color = "black",
    highlight = NULL, highlight_shape = 16, highlight_size = 3,
    highlight_color = "red", highlight_alpha = 1,
    xtrans = "identity", ytrans = "identity",
    theme = "theme_ggforge", theme_args = list(),
    palette = NULL,
    palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL,
    xlab = NULL, ylab = NULL,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
    ...) {
  # Determine default alpha based on shape (before validation)
  if (is.null(alpha)) {
    alpha <- if (shape %in% 21:25) 0.65 else 1
  }

  # Determine default palette based on color_by type
  if (is.null(palette)) {
    palette <- if (!is.null(color_by) && !is.numeric(data[[color_by]])) {
      "Paired"
    } else {
      "Spectral"
    }
  }

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
    atomic_fn = ScatterPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}
