#' Calculate Annotation Item
#'
#' @description
#' Helper function to calculate and format individual annotation items
#' for correlation plots. Supports equation, R², p-value, and correlation coefficients.
#'
#' @param item Annotation item type: "eq", "r2", "p", "spearman", "pearson", "kendall", "n"
#' @param dat Data frame with x and y variables
#' @param x X variable name
#' @param y Y variable name
#' @param m Linear model object (lm result)
#' @return Character string of formatted annotation expression (plotmath format)
#' @keywords internal
#' @importFrom stats coef cor
calculate_annotation <- function(item, dat, x, y, m) {
  if (item == "eq") {
    coefs <- stats::coef(m)
    if (is.na(coefs[2])) {
      return(as.character(as.expression(substitute(italic(y) == "NaN"))))
    }
    a <- format(as.numeric(coefs[1]), digits = 2)
    b <- format(as.numeric(abs(coefs[2])), digits = 2)
    if (coefs[2] >= 0) {
      anno_eq <- substitute(italic(y) == a + b %.% italic(x), list(a = a, b = b))
    } else {
      anno_eq <- substitute(italic(y) == a - b %.% italic(x), list(a = a, b = b))
    }
    return(as.character(as.expression(anno_eq)))
  } else if (item == "r2") {
    r2 <- format(summary(m)$r.squared, digits = 2)
    anno_r2 <- substitute(italic(R)^2 ~ "=" ~ r2, list(r2 = r2))
    return(as.character(as.expression(anno_r2)))
  } else if (item == "p") {
    coefs <- summary(m)$coefficients
    if (nrow(coefs) < 2 || all(is.na(coefs[2, ]))) {
      anno_p <- substitute(italic(P) ~ "=" ~ "NA")
    } else {
      pval <- format(coefs[2, 4], digits = 2)
      anno_p <- substitute(italic(P) ~ "=" ~ pvalue, list(pvalue = pval))
    }
    return(as.character(as.expression(anno_p)))
  } else if (item == "spearman") {
    rho <- stats::cor(dat[[x]], dat[[y]], method = "spearman", use = "complete.obs")
    anno_rho <- substitute(
      italic("Spearman's") ~ italic(rho) ~ "=" ~ value,
      list(value = format(rho, digits = 2))
    )
    return(as.character(as.expression(anno_rho)))
  } else if (item == "pearson") {
    r <- stats::cor(dat[[x]], dat[[y]], method = "pearson", use = "complete.obs")
    anno_r <- substitute(
      italic("Pearson's") ~ italic(r) ~ "=" ~ value,
      list(value = format(r, digits = 2))
    )
    return(as.character(as.expression(anno_r)))
  } else if (item == "kendall") {
    tau <- stats::cor(dat[[x]], dat[[y]], method = "kendall", use = "complete.obs")
    anno_tau <- substitute(
      italic("Kendall's") ~ italic(tau) ~ "=" ~ value,
      list(value = format(tau, digits = 2))
    )
    return(as.character(as.expression(anno_tau)))
  } else if (item == "n") {
    n <- sum(stats::complete.cases(dat[[x]], dat[[y]]))
    anno_n <- substitute(italic(N) ~ "=" ~ value, list(value = n))
    return(as.character(as.expression(anno_n)))
  } else {
    stop(
      "Unknown annotation item: ", item,
      ". Expected: eq, r2, p, spearman, pearson, kendall, n",
      call. = FALSE
    )
  }
}

#' Add Point Layers (Normal + Highlight)
#'
#' @description
#' Unified function to add point layers with optional highlighting.
#' Handles both regular geom_point and raster mode (scattermore) for large datasets.
#' Highlighted points are rendered on top with custom styling.
#'
#' @param p ggplot object
#' @param data Data frame with .highlight column
#' @param x X variable name
#' @param y Y variable name
#' @param group_by Grouping variable for coloring
#' @param pt_size Point size for normal points
#' @param pt_shape Point shape (0-25)
#' @param alpha Alpha transparency for normal points
#' @param raster Use raster graphics (scattermore)
#' @param raster_dpi DPI for raster mode
#' @param highlight_color Color for highlighted points
#' @param highlight_size Size for highlighted points
#' @param highlight_alpha Alpha for highlighted points
#' @param highlight_stroke Stroke width for highlighted points
#' @return ggplot object with point layers added
#' @keywords internal
#' @importFrom rlang sym
#' @importFrom ggplot2 geom_point aes
add_point_layers <- function(p, data, x, y, group_by,
                             pt_size, pt_shape, alpha,
                             raster, raster_dpi,
                             highlight_color, highlight_size,
                             highlight_alpha, highlight_stroke) {
  normal_data <- data[!data$.highlight, , drop = FALSE]
  highlight_data <- data[data$.highlight, , drop = FALSE]

  if (isTRUE(raster)) {
    # Raster mode
    if (nrow(normal_data) > 0) {
      p <- p + scattermore::geom_scattermore(
        data = normal_data,
        aes(color = !!sym(group_by)),
        pointsize = ceiling(pt_size),
        alpha = alpha,
        pixels = raster_dpi
      )
    }

    if (nrow(highlight_data) > 0) {
      p <- p +
        scattermore::geom_scattermore(
          data = highlight_data,
          aes(x = !!sym(x), y = !!sym(y)),
          color = highlight_color,
          pointsize = floor(highlight_size) + highlight_stroke,
          alpha = highlight_alpha,
          pixels = raster_dpi,
          inherit.aes = FALSE
        ) +
        scattermore::geom_scattermore(
          data = highlight_data,
          aes(color = !!sym(group_by)),
          pointsize = floor(highlight_size),
          alpha = highlight_alpha,
          pixels = raster_dpi
        )
    }
  } else {
    # Normal mode
    if (nrow(normal_data) > 0) {
      p <- p + geom_point(
        data = normal_data,
        aes(color = !!sym(group_by)),
        size = pt_size,
        shape = pt_shape,
        alpha = alpha
      )
    }

    if (nrow(highlight_data) > 0) {
      p <- p +
        geom_point(
          data = highlight_data,
          aes(x = !!sym(x), y = !!sym(y)),
          color = highlight_color,
          size = highlight_size + highlight_stroke,
          shape = pt_shape,
          alpha = highlight_alpha,
          inherit.aes = FALSE
        ) +
        geom_point(
          data = highlight_data,
          aes(color = !!sym(group_by)),
          size = highlight_size,
          shape = pt_shape,
          alpha = highlight_alpha
        )
    }
  }

  return(p)
}

#' Correlation Plot Atomic
#'
#' @description
#' Creates a single scatter correlation plot for two variables without splitting.
#'
#' @inheritParams parameters
#' @param x Column name for x-axis (numeric)
#' @param y Column name for y-axis (numeric)
#' @param group_by Column name for grouping. Different groups will be colored differently.
#' @param group_by_sep Separator for concatenating multiple columns in group_by
#' @param group_name Name for the group legend
#' @param pt_size Size of the points
#' @param pt_shape Shape of the points (0-25)
#' @param raster Whether to use raster graphics (faster for large datasets)
#' @param raster_dpi DPI for raster graphics as c(width, height)
#' @param highlight Items to highlight. Can be:
#'   - A vector of row indices
#'   - A vector of rownames
#'   - An expression to filter (e.g., "Species == 'setosa'")
#' @param highlight_color Color for highlighted points
#' @param highlight_size Size for highlighted points
#' @param highlight_alpha Alpha for highlighted points
#' @param highlight_stroke Stroke width for highlighted points
#' @param anno_items Annotation items to display. Options: "eq", "r2", "p", "spearman", "pearson", "kendall", "n"
#' @param anno_size Size of annotation text
#' @param anno_fg Foreground color of annotation text
#' @param anno_bg Background color of annotation text
#' @param anno_bg_r Radius of annotation background
#' @param anno_position Position of annotations. Options: "auto", "topleft", "topright", "bottomleft", "bottomright" (or shortcuts: "tl", "tr", "bl", "br")
#' @param add_smooth Whether to add linear regression line
#' @param smooth_color Color of regression line
#' @param smooth_width Width of regression line
#' @param smooth_se Whether to show standard error band
#'
#' @return A ggplot object
#' @keywords internal
#' @importFrom stats cor lm coef complete.cases
#' @importFrom rlang syms sym "%||%"
#' @importFrom dplyr group_by group_modify mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth scale_color_manual labs theme waiver
#' @importFrom ggrepel geom_text_repel
CorPlotAtomic <- function(
    data, x, y,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    pt_size = 2,
    pt_shape = 16,
    alpha = 1,
    raster = FALSE,
    raster_dpi = c(512, 512),
    highlight = NULL,
    highlight_color = "black",
    highlight_size = 1,
    highlight_alpha = 1,
    highlight_stroke = 0.8,
    anno_items = c("n", "p", "pearson"),
    anno_size = 3.5,
    anno_fg = "black",
    anno_bg = "white",
    anno_bg_r = 0.1,
    anno_position = "auto",
    add_smooth = TRUE,
    smooth_color = "red2",
    smooth_width = 1.5,
    smooth_se = FALSE,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    ...) {
  # Get ggplot function with gglogger support
  ggplot <- get_ggplot()

  # Validate anno_position
  valid_positions <- c("auto", "topleft", "topright", "bottomleft", "bottomright", "tl", "tr", "bl", "br")
  if (!anno_position %in% valid_positions) {
    stop(
      sprintf("'anno_position' must be one of: %s", paste(valid_positions, collapse = ", ")),
      call. = FALSE
    )
  }

  # Convert shortcuts
  anno_position <- switch(anno_position,
    tl = "topleft",
    tr = "topright",
    bl = "bottomleft",
    br = "bottomright",
    anno_position
  )

  # Normalize raster_dpi
  if (length(raster_dpi) == 1) {
    raster_dpi <- rep(raster_dpi, 2)
  }

  # Validate columns
  x <- validate_columns(data, x)
  y <- validate_columns(data, y)
  group_by <- validate_columns(
    data, group_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = group_by_sep
  )

  # Store whether we have a real group_by for legend positioning
  has_real_group <- !is.null(group_by)

  # Handle NULL group_by by creating a dummy group for consistent behavior
  if (is.null(group_by)) {
    group_by <- ".group"
    data[[group_by]] <- factor("")
  }

  # Normalize legend position based on whether a real group was provided
  legend.position <- normalize_legend_position(
    legend.position,
    has_group = has_real_group
  )

  # Calculate text size scale
  base_size <- theme_args$base_size %||% 12
  text_size_scale <- base_size / 12

  # Calculate annotations per facet group (needed for faceted plots)
  annodata <- data %>%
    dplyr::group_by(!!!syms(facet_by)) %>%
    dplyr::group_modify(function(dat, g) {
      m <- stats::lm(dat[[y]] ~ dat[[x]])

      # Keep each annotation as a separate row for proper spacing
      anno_list <- lapply(anno_items, function(item) {
        calculate_annotation(item, dat, x, y, m)
      })

      # Calculate correlation for auto positioning (positive -> topleft, negative -> topright)
      corr_val <- stats::cor(dat[[x]], dat[[y]], method = "pearson", use = "complete.obs")

      data.frame(
        anno = unlist(anno_list),
        corr = corr_val,
        stringsAsFactors = FALSE
      )
    })

  # Auto position: positive correlation -> topleft, negative -> topright
  if (anno_position == "auto") {
    first_corr <- annodata$corr[1]
    if (!is.na(first_corr) && first_corr >= 0) {
      anno_position <- "topleft"
    } else {
      anno_position <- "topright"
    }
  }

  # Process highlight: supports TRUE, expression strings, or row indices/names
  data$.highlight <- FALSE
  if (!is.null(highlight)) {
    if (isTRUE(highlight)) {
      # Highlight all points
      data$.highlight <- TRUE
    } else if (length(highlight) == 1 && is.character(highlight)) {
      # Expression filter (e.g., 'Species == "setosa"')
      data <- data %>% dplyr::mutate(.highlight = eval(parse(text = highlight)))
    } else {
      # Row indices or names
      all_inst <- rownames(data) %||% seq_len(nrow(data))
      if (!any(highlight %in% all_inst)) {
        stop("No highlight items found in the data (rownames).", call. = FALSE)
      }
      if (!all(highlight %in% all_inst)) {
        warning("Some highlight items not found in the data (rownames).", call. = FALSE)
      }
      data$.highlight <- all_inst %in% highlight
      rm(all_inst)
    }
  }

  # Initialize plot
  p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))

  # Add points
  p <- add_point_layers(
    p, data, x, y, group_by,
    pt_size, pt_shape, alpha,
    raster, raster_dpi,
    highlight_color, highlight_size,
    highlight_alpha, highlight_stroke
  )

  # Add smooth line if requested
  if (add_smooth) {
    p <- p + geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = smooth_se,
      color = smooth_color,
      linewidth = smooth_width,
      alpha = 0.5
    )
  }

  # Add annotations (each as separate row, repel will space them vertically)
  anno_x <- if (grepl("left", anno_position)) -Inf else Inf
  anno_y <- if (grepl("top", anno_position)) Inf else -Inf
  anno_hjust <- if (grepl("left", anno_position)) 0 else 1

  p <- p +
    ggrepel::geom_text_repel(
      data = annodata,
      parse = TRUE,
      hjust = anno_hjust,
      direction = "y",
      aes(label = !!sym("anno")),
      x = anno_x,
      y = anno_y,
      size = text_size_scale * anno_size,
      bg.color = anno_bg,
      bg.r = anno_bg_r,
      color = anno_fg,
      min.segment.length = 0,
      max.overlaps = 100,
      force = 0.5,
      box.padding = 0.3,
      point.padding = 0.3,
      segment.color = "transparent"
    )

  # Add color scale
  p <- p + scale_color_manual(
    name = group_name %||% group_by,
    values = get_palette(
      levels(data[[group_by]]),
      palette = palette,
      palcolor = palcolor
    )
  )

  # Add labels
  p <- p + labs(
    title = title,
    subtitle = subtitle,
    x = xlab %||% x,
    y = ylab %||% y
  )

  # Apply theme
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(aspect.ratio = aspect.ratio)

  # Apply data-driven styling (both x and y are continuous numeric variables)
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

  # Add faceting if requested
  p <- add_facets(
    p,
    facet_by = facet_by,
    facet_scales = facet_scales,
    nrow = facet_nrow,
    ncol = facet_ncol,
    byrow = facet_byrow
  )

  # Set plot dimensions as attributes
  height <- width <- 4.5
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

  return(p)
}

#' Correlation Plot
#'
#' @description
#' Generate scatter correlation plot for two variables with optional linear
#' regression line, annotations, and highlighting.
#'
#' @inheritParams parameters
#' @inheritParams CorPlotAtomic
#'
#' @return A ggplot object or list/combined plots if split_by is used
#' @export
#' @examples
#' # Basic correlation plot with grouping
#' data(iris)
#' CorPlot(iris, x = "Sepal.Length", y = "Sepal.Width", group_by = "Species")
#'
#' # With custom annotations and positioning
#' CorPlot(iris,
#'   x = "Sepal.Length", y = "Sepal.Width",
#'   group_by = "Species",
#'   anno_items = c("n", "eq", "r2", "pearson"),
#'   anno_position = "bottomright"
#' )
#'
#' # With highlighting specific points
#' CorPlot(iris,
#'   x = "Sepal.Length", y = "Sepal.Width",
#'   group_by = "Species",
#'   highlight = 'Species == "setosa"',
#'   highlight_color = "red",
#'   highlight_size = 3,
#'   highlight_stroke = 1.5
#' )
#'
#' # With faceting by groups
#' CorPlot(iris,
#'   x = "Sepal.Length", y = "Sepal.Width",
#'   facet_by = "Species",
#'   facet_scales = "free",
#'   add_smooth = TRUE,
#'   smooth_color = "blue"
#' )
#'
#' # With splitting and custom palettes
#' CorPlot(iris,
#'   x = "Sepal.Length", y = "Sepal.Width",
#'   split_by = "Species",
#'   palette = c(setosa = "Set1", versicolor = "Dark2", virginica = "Paired"),
#'   combine = TRUE
#' )
#'
#' # For large datasets, use raster mode
#' \dontrun{
#' CorPlot(large_data,
#'   x = "x_var", y = "y_var",
#'   raster = TRUE,
#'   raster_dpi = c(1024, 1024),
#'   pt_size = 1
#' )
#' }
CorPlot <- function(
    data, x, y,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    split_by = NULL,
    split_by_sep = "_",
    pt_size = 2,
    pt_shape = 16,
    raster = FALSE,
    alpha = 1,
    raster_dpi = c(512, 512),
    highlight = NULL,
    highlight_color = "black",
    highlight_size = 1,
    highlight_alpha = 1,
    highlight_stroke = 0.8,
    anno_items = c("n", "p", "pearson"),
    anno_size = 3.5,
    anno_fg = "black",
    anno_bg = "white",
    anno_bg_r = 0.1,
    anno_position = "auto",
    add_smooth = TRUE,
    smooth_color = "red2",
    smooth_width = 1.5,
    smooth_se = FALSE,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
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
    atomic_fn = CorPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}
