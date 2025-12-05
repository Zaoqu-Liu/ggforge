#' Dot Plot / Scatter Plot
#'
#' @description
#' Creates a dot plot where both X and Y axes can be numeric or categorical.
#' When both are numeric, the plot functions as a scatter plot.
#' Supports sizing dots by a numeric column and filling by another numeric column.
#'
#' @inheritParams parameters
#' @param x A character string specifying the column to use for the x-axis.
#'   Can be numeric or factor/character. When multiple columns are provided,
#'   they will be concatenated with `x_sep`.
#' @param y A character string specifying the column to use for the y-axis.
#'   Can be numeric or factor/character. When multiple columns are provided,
#'   they will be concatenated with `y_sep`.
#' @param x_sep A character string to concatenate multiple columns in x.
#' @param y_sep A character string to concatenate multiple columns in y.
#' @param size_by Which column to use as the size of the dots (numeric column).
#'   If not provided, the size will be the count of instances for each (x, y) pair.
#'   Can also be a single numeric value to specify a fixed size.
#' @param fill_by Which column to use to fill the dots (numeric column).
#'   If not provided, all dots will be filled with the middle color of the palette.
#' @param fill_cutoff A numeric value specifying the cutoff for the fill column.
#'   Values below (or above if `fill_reverse = TRUE`) will be shown in grey.
#' @param fill_reverse Whether to reverse the fill direction. If FALSE (default),
#'   values < cutoff are grey. If TRUE, values > cutoff are grey.
#' @param size_name A character string to name the size legend.
#' @param fill_name A character string to name the fill legend.
#' @param fill_cutoff_name A character string to name the fill cutoff legend.
#' @param flip Whether to flip the x and y axes.
#' @param add_bg Whether to add a background color to the plot.
#' @param bg_palette Palette for the background color.
#' @param bg_palcolor Custom colors for the background.
#' @param bg_alpha Alpha value for the background color.
#' @param bg_direction Direction for background stripes ("vertical" or "horizontal").
#' @param lollipop Whether to create a lollipop plot (requires numeric x and factor y).
#' @param x_text_angle Angle for x-axis text.
#' @param keep_empty Whether to keep empty factor levels.
#'
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' \donttest{
#' mtcars <- datasets::mtcars
#' mtcars$carb <- factor(mtcars$carb)
#' mtcars$gear <- factor(mtcars$gear)
#' DotPlot(mtcars,
#'   x = "carb", y = "gear", size_by = "wt",
#'   fill_by = "mpg", fill_cutoff = 18
#' )
#' DotPlot(mtcars,
#'   x = "carb", y = "gear", size_by = "wt",
#'   fill_by = "mpg", fill_cutoff = 18, add_bg = TRUE
#' )
#' # Scatter plot (both axes numeric)
#' DotPlot(mtcars,
#'   x = "qsec", y = "drat", size_by = "wt",
#'   fill_by = "mpg", fill_cutoff = 18
#' )
#' }
DotPlot <- function(
    data, x, y,
    x_sep = "_",
    y_sep = "_",
    flip = FALSE,
    split_by = NULL,
    split_by_sep = "_",
    size_by = NULL,
    fill_by = NULL,
    fill_cutoff = NULL,
    fill_reverse = FALSE,
    size_name = NULL,
    fill_name = NULL,
    fill_cutoff_name = NULL,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    bg_direction = c("vertical", "horizontal", "v", "h"),
    lollipop = FALSE,
    theme = "theme_ggforge_grid",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    alpha = 1,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    x_text_angle = 0,
    seed = 8525,
    aspect.ratio = NULL,
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

  # Validate bg_direction
  bg_direction <- match.arg(bg_direction)
  if (bg_direction %in% c("h", "horizontal")) {
    bg_direction <- "horizontal"
  } else {
    bg_direction <- "vertical"
  }

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
    atomic_fn = DotPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Dot Plot Atomic
#'
#' @description
#' Creates a single dot plot without splitting
#'
#' @inheritParams DotPlot
#' @keywords internal
#' @importFrom ggplot2 aes geom_point scale_size_area scale_fill_gradientn
#' @importFrom ggplot2 scale_color_manual scale_x_discrete scale_y_discrete
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous coord_flip
#' @importFrom ggplot2 guide_colorbar guide_legend guide_none guides labs theme
#' @importFrom ggplot2 element_line element_text geom_segment scale_color_gradientn
#' @importFrom ggnewscale new_scale_color
#' @importFrom rlang sym syms "%||%"
DotPlotAtomic <- function(
    data, x, y,
    x_sep = "_",
    y_sep = "_",
    flip = FALSE,
    lollipop = FALSE,
    size_by = NULL,
    fill_by = NULL,
    fill_cutoff = NULL,
    fill_reverse = FALSE,
    size_name = NULL,
    fill_name = NULL,
    fill_cutoff_name = NULL,
    theme = "theme_ggforge_grid",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    alpha = 1,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    x_text_angle = 0,
    aspect.ratio = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    bg_direction = c("vertical", "horizontal", "v", "h"),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
    ...) {
  # Determine if x and y are numeric BEFORE validation
  x_is_numeric <- length(x) == 1 && !is.character(data[[x]]) && !is.factor(data[[x]])
  y_is_numeric <- length(y) == 1 && !is.character(data[[y]]) && !is.factor(data[[y]])

  # Validate and process columns
  if (!x_is_numeric) {
    x <- validate_columns(
      data, x,
      force_factor = TRUE,
      allow_multi = TRUE,
      concat_multi = TRUE,
      concat_sep = x_sep
    )
  }

  if (!y_is_numeric) {
    y <- validate_columns(
      data, y,
      force_factor = TRUE,
      allow_multi = TRUE,
      concat_multi = TRUE,
      concat_sep = y_sep
    )
  }

  # Validate fill_cutoff usage
  if (!is.null(fill_cutoff) && is.null(fill_by)) {
    stop("'fill_by' must be provided when 'fill_cutoff' is specified.", call. = FALSE)
  }

  # Validate facet_by
  facet_by <- validate_columns(
    data, facet_by,
    force_factor = TRUE,
    allow_multi = TRUE
  )

  # Handle size_by
  if (!is.numeric(size_by)) {
    size_by <- validate_columns(data, size_by)
  }

  if (is.null(size_by)) {
    if (is.null(fill_by)) {
      data <- data |>
        dplyr::group_by(!!!syms(unique(c(x, y, facet_by)))) |>
        dplyr::summarise(.size = dplyr::n(), .groups = "drop")
    } else {
      # Aggregate data and check if there are duplicates
      data <- data |>
        dplyr::group_by(!!!syms(unique(c(x, y, facet_by)))) |>
        dplyr::summarise(!!sym(fill_by) := dplyr::first(!!sym(fill_by)), .size = dplyr::n(), .groups = "drop")

      # Only warn if there are actually duplicates (count > 1)
      if (any(data$.size > 1)) {
        warning("Using the first value of fill_by as size_by is calculated from count of instances.", immediate. = TRUE)
      }
    }
    size_by <- ".size"
    # Set default size_name if not provided
    if (is.null(size_name)) {
      size_name <- "Count"
    }
  }

  # Handle fill_by
  fill_by <- validate_columns(data, fill_by)

  if (!is.null(fill_by) && !is.null(fill_cutoff)) {
    # Add a column to indicate the fill cutoff
    if (isFALSE(fill_reverse)) {
      fill_cutoff_label <- paste0(fill_by, " < ", fill_cutoff)
      data[[fill_by]][data[[fill_by]] < fill_cutoff] <- NA
    } else {
      fill_cutoff_label <- paste0(fill_by, " > ", fill_cutoff)
      data[[fill_by]][data[[fill_by]] > fill_cutoff] <- NA
    }
  }

  if (is.null(fill_by)) {
    data$.fill_by <- 1
    fill_by <- ".fill_by"
    fill_legend <- FALSE
  } else {
    fill_legend <- TRUE
  }

  # Get ggplot (support gglogger)
  ggplot <- get_ggplot()

  # Calculate text justification
  just <- calc_justification(x_text_angle)

  # Build plot
  p <- ggplot(data, ggplot2::aes(x = !!sym(x), y = !!sym(y)))

  # Add background layer if requested
  if (isTRUE(add_bg)) {
    if (bg_direction %in% c("vertical", "v")) {
      if (x_is_numeric) {
        stop("Vertical 'bg_direction' is not supported when 'x' is numeric.", call. = FALSE)
      }
      p <- p + create_bg_layer(data, x, bg_palette, bg_palcolor, bg_alpha, keep_empty, facet_by, "vertical")
    } else {
      if (y_is_numeric) {
        stop("Horizontal 'bg_direction' is not supported when 'y' is numeric.", call. = FALSE)
      }
      p <- p + create_bg_layer(data, y, bg_palette, bg_palcolor, bg_alpha, keep_empty, facet_by, "horizontal")
    }
  }

  # Add scale for x and y
  if (!x_is_numeric) {
    p <- p + ggplot2::scale_x_discrete(drop = !keep_empty)
  }
  if (!y_is_numeric) {
    p <- p + ggplot2::scale_y_discrete(drop = !keep_empty)
  }

  # Add lollipop if requested
  if (isTRUE(lollipop)) {
    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = !!sym(x), yend = !!sym(y)), color = "black", linewidth = 2) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = !!sym(x), yend = !!sym(y), color = !!sym(fill_by)), linewidth = 1) +
      ggplot2::scale_x_continuous(expand = c(0, 0, 0.05, 0)) +
      ggplot2::scale_color_gradientn(
        n.breaks = 5,
        colors = get_palette(
          1:100,
          palette = palette,
          palcolor = palcolor,
          type = "continuous",
          reverse = fill_reverse
        ),
        na.value = "grey80",
        guide = "none"
      ) +
      ggnewscale::new_scale_color()
  }

  # Add points with size and fill
  if (is.numeric(size_by)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(fill = !!sym(fill_by), color = ""),
      size = size_by,
      shape = 21,
      alpha = alpha
    )
  } else {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(size = !!sym(size_by), fill = !!sym(fill_by), color = ""),
        shape = 21,
        alpha = alpha
      ) +
      ggplot2::scale_size_area(max_size = 6, n.breaks = 4) +
      ggplot2::guides(size = ggplot2::guide_legend(
        title = size_name %||% size_by,
        override.aes = list(fill = "transparent", shape = 21, colour = "black"),
        order = 1
      ))
  }

  # Add fill scale
  p <- p +
    ggplot2::scale_fill_gradientn(
      n.breaks = 5,
      colors = get_palette(
        1:100,
        palette = palette,
        palcolor = palcolor,
        type = "continuous",
        reverse = fill_reverse
      ),
      na.value = "grey80",
      guide = if (isTRUE(fill_legend)) {
        ggplot2::guide_colorbar(
          title = fill_name %||% fill_by,
          frame.colour = "black",
          ticks.colour = "black",
          frame.linewidth = 0.3,
          ticks.linewidth = 0.3,
          title.hjust = 0,
          order = 2
        )
      } else {
        ggplot2::guide_none()
      }
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
    x_var = if (flip) y else x,
    y_var = if (flip) x else y,
    flip = FALSE, # Already handled above
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction,
    axis.text.x = ggplot2::element_text(
      angle = x_text_angle,
      hjust = just$h,
      vjust = just$v
    )
  )

  # Add color scale and fill cutoff guide
  p <- p + ggplot2::scale_color_manual(values = "black", na.value = "black", guide = "none")

  if (!is.null(fill_by) && !is.null(fill_cutoff) && anyNA(data[[fill_by]])) {
    p <- p + ggplot2::guides(color = ggplot2::guide_legend(
      title = fill_cutoff_name %||% fill_cutoff_label,
      override.aes = list(colour = "black", fill = "grey80", size = 3),
      order = 3
    ))
  }

  # Flip coordinates if requested
  if (isTRUE(flip)) {
    p <- p + ggplot2::coord_flip()
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

#' Lollipop Plot
#'
#' @description
#' Creates a lollipop plot with a numeric x-axis and categorical y-axis.
#' This is a convenience wrapper around `DotPlot` with `lollipop = TRUE`.
#'
#' @inheritParams DotPlot
#' @param x A character string specifying the column to use for the x-axis
#'   (numeric column expected).
#' @param y A character string specifying the column to use for the y-axis
#'   (factor/character column expected).
#'
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' \donttest{
#' mtcars <- datasets::mtcars
#' LollipopPlot(mtcars,
#'   x = "qsec", y = "gear", size_by = "wt",
#'   fill_by = "mpg"
#' )
#' LollipopPlot(mtcars,
#'   x = "qsec", y = "gear", size_by = "wt",
#'   fill_by = "mpg", fill_cutoff = 18
#' )
#' }
LollipopPlot <- function(
    data, x, y,
    y_sep = "_",
    flip = FALSE,
    split_by = NULL,
    split_by_sep = "_",
    size_by = NULL,
    fill_by = NULL,
    fill_cutoff = NULL,
    fill_reverse = FALSE,
    size_name = NULL,
    fill_name = NULL,
    fill_cutoff_name = NULL,
    theme = "theme_ggforge_grid",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    alpha = 1,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    x_text_angle = 0,
    seed = 8525,
    aspect.ratio = NULL,
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
    axes = NULL,
    axis_titles = NULL,
    guides = NULL,
    design = NULL,
    ...) {
  DotPlot(
    data = data,
    x = x,
    y = y,
    x_sep = NULL,
    y_sep = y_sep,
    flip = flip,
    lollipop = TRUE,
    split_by = split_by,
    split_by_sep = split_by_sep,
    size_by = size_by,
    fill_by = fill_by,
    fill_cutoff = fill_cutoff,
    fill_reverse = fill_reverse,
    size_name = size_name,
    fill_name = fill_name,
    fill_cutoff_name = fill_cutoff_name,
    theme = theme,
    theme_args = theme_args,
    palette = palette,
    palcolor = palcolor,
    alpha = alpha,
    facet_by = facet_by,
    facet_scales = facet_scales,
    facet_ncol = facet_ncol,
    facet_nrow = facet_nrow,
    facet_byrow = facet_byrow,
    x_text_angle = x_text_angle,
    seed = seed,
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction,
    title = title,
    subtitle = subtitle,
    xlab = xlab,
    ylab = ylab,
    keep_empty = keep_empty,
    combine = combine,
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    axes = axes,
    axis_titles = axis_titles,
    guides = guides,
    design = design,
    ...
  )
}
