#' Dimension Reduction Plot
#'
#' @description
#' Visualizes dimension reduction data (UMAP, t-SNE, PCA, etc.) as scatter plots.
#' Supports both categorical grouping and continuous feature plotting with extensive
#' customization options.
#'
#' @inheritParams parameters
#' @param dims Column names or indices for x and y axes. Default is first two columns.
#' @param group_by Column name for categorical grouping. Creates discrete color mapping.
#'   If multiple columns provided, they will be concatenated with `group_by_sep`.
#' @param group_by_sep Separator for concatenating multiple `group_by` columns.
#' @param pt_size Point size. If NULL, calculated based on number of data points.
#' @param pt_alpha Point transparency (0-1).
#' @param bg_color Color for NA/background points.
#' @param label Whether to show group labels at median positions.
#' @param label_insitu Whether to use actual group names as labels (vs. numbers).
#' @param label_size Size of labels.
#' @param label_fg,label_bg Foreground and background colors for labels.
#' @param label_bg_r Background radius for labels.
#' @param label_repel Whether to repel overlapping labels.
#' @param label_repulsion Repulsion force for labels.
#' @param label_pt_size,label_pt_color Point size and color for label anchors.
#' @param label_segment_color Color of segments connecting labels to points.
#' @param show_stat Whether to show sample count in subtitle.
#' @param order How to order points: "as-is", "reverse", "high-top", "low-top", "random".
#'   Affects drawing order (what's on top).
#' @param highlight Row names/indices to highlight, or logical expression as string.
#' @param highlight_alpha,highlight_size,highlight_color,highlight_stroke
#'   Styling for highlighted points.
#' @param add_mark Whether to add marks (hulls/ellipses) around groups.
#' @param mark_type Type of mark: "hull", "ellipse", "rect", "circle".
#' @param mark_expand,mark_alpha,mark_linetype Mark styling parameters.
#' @param add_density Whether to add 2D density contours/fill.
#' @param density_color,density_filled Density styling.
#' @param density_filled_palette,density_filled_palcolor Palette for filled density.
#' @param raster Whether to rasterize points (useful for large datasets).
#' @param raster_dpi DPI for rasterization.
#' @param hex Whether to use hexagonal binning instead of points.
#' @param hex_linewidth,hex_count,hex_bins,hex_binwidth Hex bin parameters.
#'
#' @return A ggplot object or list/combined plots
#' @export
#' @examples
#' \dontrun{
#' # Basic usage
#' DimPlot(data, dims = c("UMAP_1", "UMAP_2"), group_by = "clusters")
#'
#' # With faceting
#' DimPlot(data, dims = 1:2, group_by = "clusters", facet_by = "condition")
#'
#' # With splitting
#' DimPlot(data, dims = 1:2, group_by = "clusters", split_by = "timepoint")
#' }
DimPlot <- function(
    data, dims = 1:2, group_by = NULL, group_by_sep = "_",
    split_by = NULL, split_by_sep = "_",
    pt_size = NULL, pt_alpha = 1, bg_color = "grey80",
    label = FALSE, label_insitu = FALSE, label_size = 4,
    label_fg = "white", label_bg = "black", label_bg_r = 0.1,
    label_repel = FALSE, label_repulsion = 20,
    label_pt_size = 1, label_pt_color = "black", label_segment_color = "black",
    show_stat = !identical(theme, "theme_blank"),
    order = c("as-is", "reverse", "high-top", "low-top", "random"),
    highlight = NULL, highlight_alpha = 1, highlight_size = 1,
    highlight_color = "black", highlight_stroke = 0.8,
    add_mark = FALSE, mark_type = c("hull", "ellipse", "rect", "circle"),
    mark_expand = grid::unit(3, "mm"), mark_alpha = 0.1, mark_linetype = 1,
    add_density = FALSE, density_color = "grey80", density_filled = FALSE,
    density_filled_palette = "Greys", density_filled_palcolor = NULL,
    raster = NULL, raster_dpi = c(512, 512),
    hex = FALSE, hex_linewidth = 0.5, hex_count = TRUE,
    hex_bins = 50, hex_binwidth = NULL,
    facet_by = NULL, facet_scales = "fixed",
    facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    theme = "theme_ggforge_grid", theme_args = list(),
    palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    keep_empty = FALSE, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = NULL, guides = NULL, design = NULL,
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

  # Validate splits
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
    atomic_fn = DimPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Dimension Reduction Plot Atomic
#'
#' @description
#' Creates a single dimension reduction plot without splitting
#'
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_point geom_density_2d stat_density_2d
#' @importFrom ggplot2 geom_hex stat_summary_hex geom_segment scale_linewidth_continuous
#' @importFrom ggplot2 scale_color_manual scale_fill_manual scale_color_gradientn
#' @importFrom ggplot2 scale_fill_gradientn scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 labs theme element_text guide_legend guide_colorbar
#' @importFrom ggplot2 annotation_custom ggplotGrob arrow unit waiver layer_scales
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr %>% group_by summarise n filter arrange desc distinct
#' @importFrom tidyr pivot_longer complete expand_grid
#' @importFrom rlang sym syms := "%||%"
#' @importFrom scales rescale
#' @importFrom stats aggregate median
DimPlotAtomic <- function(
    data, dims = 1:2, group_by = NULL, group_by_sep = "_",
    pt_size = NULL, pt_alpha = 1, bg_color = "grey80",
    label = FALSE, label_insitu = FALSE, label_size = 4,
    label_fg = "white", label_bg = "black", label_bg_r = 0.1,
    label_repel = FALSE, label_repulsion = 20,
    label_pt_size = 1, label_pt_color = "black", label_segment_color = "black",
    show_stat = !identical(theme, "theme_blank"),
    order = c("as-is", "reverse", "high-top", "low-top", "random"),
    highlight = NULL, highlight_alpha = 1, highlight_size = 1,
    highlight_color = "black", highlight_stroke = 0.8,
    add_mark = FALSE, mark_type = c("hull", "ellipse", "rect", "circle"),
    mark_expand = grid::unit(3, "mm"), mark_alpha = 0.1, mark_linetype = 1,
    add_density = FALSE, density_color = "grey80", density_filled = FALSE,
    density_filled_palette = "Greys", density_filled_palcolor = NULL,
    raster = NULL, raster_dpi = c(512, 512),
    hex = FALSE, hex_linewidth = 0.5, hex_count = TRUE,
    hex_bins = 50, hex_binwidth = NULL,
    facet_by = NULL, facet_scales = "fixed",
    facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    theme = "theme_ggforge_grid", theme_args = list(),
    palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    keep_empty = FALSE, ...) {
  # Get ggplot function (with gglogger support)
  ggplot <- get_ggplot()

  # Match order argument
  order <- match.arg(order)

  # Process dims (convert indices to column names)
  if (is.numeric(dims)) {
    if (max(dims) > ncol(data)) {
      stop(sprintf("dims indices exceed number of columns (%d)", ncol(data)), call. = FALSE)
    }
    dims <- colnames(data)[dims]
  }

  if (length(dims) != 2) {
    stop("Only 2 dimensions are allowed for dimension reduction plot.", call. = FALSE)
  }

  # Validate columns
  dims <- validate_columns(data, dims, allow_multi = TRUE)
  group_by <- validate_columns(
    data, group_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = group_by_sep
  )
  facet_by <- validate_columns(data, facet_by, force_factor = TRUE)

  # Check that group_by is provided
  if (is.null(group_by)) {
    stop("'group_by' must be specified for DimPlot", call. = FALSE)
  }

  # Auto-calculate point size if not provided
  pt_size <- pt_size %||% min(3000 / nrow(data), 0.6)

  # Auto-determine whether to raster
  raster <- raster %||% (nrow(data) > 1e5)

  # Ensure raster_dpi is length 2
  if (length(raster_dpi) == 1) {
    raster_dpi <- rep(raster_dpi, 2)
  }

  # Set default labels
  xlab <- xlab %||% dims[1]
  ylab <- ylab %||% dims[2]

  # Setup labels and colors for groups
  colors <- get_palette(
    levels(data[[group_by]]),
    palette = palette,
    palcolor = palcolor,
    NA_color = bg_color
  )

  labels_tb <- table(data[[group_by]])
  labels_tb <- labels_tb[labels_tb != 0]

  # Create label text based on options
  if (isTRUE(label_insitu)) {
    if (isTRUE(show_stat)) {
      label_use <- paste0(names(labels_tb), " (", labels_tb, ")")
    } else {
      label_use <- paste0(names(labels_tb))
    }
  } else {
    if (isTRUE(label)) {
      if (isTRUE(show_stat)) {
        label_use <- paste0(seq_along(labels_tb), ": ", names(labels_tb), " (", labels_tb, ")")
      } else {
        label_use <- paste0(seq_along(labels_tb), ": ", names(labels_tb))
      }
    } else {
      if (isTRUE(show_stat)) {
        label_use <- paste0(names(labels_tb), " (", labels_tb, ")")
      } else {
        label_use <- paste0(names(labels_tb))
      }
    }
  }

  # Setup subtitle with count if requested
  if (isTRUE(show_stat) && is.null(facet_by)) {
    subtitle <- subtitle %||% paste0("N = ", sum(!is.na(data[[group_by]])))
  }

  # Apply point ordering
  if (order == "reverse") {
    data <- data[nrow(data):1, , drop = FALSE]
  } else if (order == "high-top") {
    data <- dplyr::arrange(data, !!sym(group_by))
  } else if (order == "low-top") {
    data <- dplyr::arrange(data, dplyr::desc(!!sym(group_by)))
  } else if (order == "random") {
    data <- data[sample(nrow(data)), , drop = FALSE]
  }

  # Determine if we'll use fill scale (hex with count uses fill)
  has_fill <- FALSE

  # Initialize plot
  p <- ggplot(data)

  # Add marks if requested
  if (isTRUE(add_mark)) {
    if (!requireNamespace("ggforce", quietly = TRUE)) {
      stop("'ggforce' package is required for adding marks. Install it with: install.packages('ggforce')", call. = FALSE)
    }

    mark_type <- match.arg(mark_type)
    mark_fun <- switch(mark_type,
      hull = ggforce::geom_mark_hull,
      ellipse = ggforce::geom_mark_ellipse,
      rect = ggforce::geom_mark_rect,
      circle = ggforce::geom_mark_circle
    )

    p <- p + mark_fun(
      data = data[!is.na(data[[group_by]]), , drop = FALSE],
      mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(group_by), fill = !!sym(group_by)),
      expand = mark_expand, alpha = mark_alpha, linetype = mark_linetype,
      show.legend = FALSE
    ) +
      scale_fill_manual(values = colors[names(labels_tb)]) +
      scale_color_manual(values = colors[names(labels_tb)])

    # Need to add new scales for points
    if (!requireNamespace("ggnewscale", quietly = TRUE)) {
      stop("'ggnewscale' package is required when using marks. Install it with: install.packages('ggnewscale')", call. = FALSE)
    }
    p <- p + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
  }

  # Add density plot
  if (isTRUE(add_density)) {
    if (isTRUE(density_filled)) {
      filled_color <- get_palette(
        palette = density_filled_palette,
        palcolor = density_filled_palcolor,
        type = "continuous"
      )
      p <- p +
        stat_density_2d(
          geom = "raster",
          aes(x = !!sym(dims[1]), y = !!sym(dims[2]), fill = after_stat(!!sym("density"))),
          contour = FALSE, inherit.aes = FALSE, show.legend = FALSE
        ) +
        scale_fill_gradientn(name = "Density", colours = filled_color)

      if (!requireNamespace("ggnewscale", quietly = TRUE)) {
        stop("'ggnewscale' package is required when using filled density. Install it with: install.packages('ggnewscale')", call. = FALSE)
      }
      p <- p + ggnewscale::new_scale_fill()
    } else {
      p <- p + geom_density_2d(
        aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
        color = density_color, inherit.aes = FALSE, show.legend = FALSE
      )
    }
  }

  # Get axis limits
  x_min <- min(data[[dims[1]]], na.rm = TRUE)
  x_max <- max(data[[dims[1]]], na.rm = TRUE)
  y_min <- min(data[[dims[2]]], na.rm = TRUE)
  y_max <- max(data[[dims[2]]], na.rm = TRUE)

  # Add labels and scales
  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
    ggplot2::scale_x_continuous(limits = c(x_min, x_max)) +
    ggplot2::scale_y_continuous(limits = c(y_min, y_max))

  # Apply theme and automatic styling
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(aspect.ratio = aspect.ratio)

  # Apply data-driven styling (both dims are continuous)
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = dims[1],
    y_var = dims[2],
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Add background points for facets (if faceting and not multiple features)
  if (!is.null(facet_by)) {
    # Add the points from other subplots as background
    all_fc_values <- dplyr::distinct(data, !!!syms(facet_by))
    colnames(all_fc_values) <- paste0(".facet_", colnames(all_fc_values))
    fc_data <- tidyr::expand_grid(data, all_fc_values)

    # Remove the points inside the facet
    fc_indicator <- TRUE
    for (fc in facet_by) {
      fc_indicator <- fc_indicator & (fc_data[[fc]] == fc_data[[paste0(".facet_", fc)]])
    }
    fc_data <- fc_data[!fc_indicator, , drop = FALSE]
    fc_data[, facet_by, drop = FALSE] <- fc_data[, paste0(".facet_", facet_by), drop = FALSE]

    # Add background points
    if (isTRUE(raster)) {
      if (!requireNamespace("scattermore", quietly = TRUE)) {
        stop("'scattermore' package is required for rasterization. Install it with: install.packages('scattermore')", call. = FALSE)
      }
      p <- p + scattermore::geom_scattermore(
        data = fc_data,
        mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
        size = pt_size, alpha = pt_alpha / 2, color = bg_color, pixels = raster_dpi
      )
    } else if (isTRUE(hex)) {
      p <- p + geom_hex(
        data = fc_data,
        mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
        linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth,
        fill = bg_color, alpha = pt_alpha / 2
      )
    } else {
      p <- p + geom_point(
        data = fc_data,
        mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
        size = pt_size, alpha = pt_alpha / 2, color = bg_color
      )
    }
    rm(fc_data)
  }

  # Add main points layer
  if (isTRUE(raster)) {
    if (!requireNamespace("scattermore", quietly = TRUE)) {
      stop("'scattermore' package is required for rasterization. Install it with: install.packages('scattermore')", call. = FALSE)
    }
    # Add NA points first
    p <- p + scattermore::geom_scattermore(
      data = data[is.na(data[[group_by]]), , drop = FALSE],
      mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
      color = bg_color,
      pointsize = ceiling(pt_size), alpha = pt_alpha, pixels = raster_dpi
    )
    # Then add colored points
    p <- p + scattermore::geom_scattermore(
      data = data[!is.na(data[[group_by]]), , drop = FALSE],
      mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(group_by)),
      pointsize = ceiling(pt_size), alpha = pt_alpha, pixels = raster_dpi
    )
  } else if (isTRUE(hex)) {
    if (!requireNamespace("hexbin", quietly = TRUE)) {
      stop("'hexbin' package is required for hexagonal binning. Install it with: install.packages('hexbin')", call. = FALSE)
    }
    has_fill <- TRUE
    if (isTRUE(hex_count)) {
      p <- p + geom_hex(
        mapping = aes(
          x = !!sym(dims[1]), y = !!sym(dims[2]),
          color = !!sym(group_by), fill = !!sym(group_by),
          alpha = after_stat(!!sym("count"))
        ),
        linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth
      )
    } else {
      p <- p + geom_hex(
        mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(group_by), fill = !!sym(group_by)),
        linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth
      )
    }
  } else {
    # Regular points
    p <- p + geom_point(
      mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(group_by)),
      size = pt_size, alpha = pt_alpha
    )
  }

  # Add highlighting
  if (!is.null(highlight)) {
    if (isTRUE(hex)) {
      warning("Highlight is not supported for hex plot.", immediate. = TRUE)
    } else {
      if (isTRUE(highlight)) {
        hi_df <- data
      } else if (length(highlight) == 1 && is.character(highlight)) {
        # Evaluate as expression
        hi_df <- dplyr::filter(data, eval(parse(text = highlight)))
      } else {
        all_inst <- if (is.numeric(highlight)) 1:nrow(data) else rownames(data)
        if (!any(highlight %in% all_inst)) {
          stop("No highlight items found in the data (rownames).", call. = FALSE)
        }
        if (!all(highlight %in% all_inst)) {
          warning("Not all highlight items found in the data (rownames).", immediate. = TRUE)
        }
        hi_df <- data[intersect(highlight, all_inst), , drop = FALSE]
      }

      if (nrow(hi_df) > 0) {
        if (isTRUE(raster)) {
          p <- p +
            scattermore::geom_scattermore(
              data = hi_df,
              aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
              color = highlight_color,
              pointsize = floor(highlight_size) + highlight_stroke,
              alpha = highlight_alpha, pixels = raster_dpi
            ) +
            scattermore::geom_scattermore(
              data = hi_df,
              aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(group_by)),
              pointsize = floor(highlight_size),
              alpha = highlight_alpha, pixels = raster_dpi
            )
        } else {
          p <- p +
            geom_point(
              data = hi_df,
              aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
              color = highlight_color,
              size = highlight_size + highlight_stroke,
              alpha = highlight_alpha
            ) +
            geom_point(
              data = hi_df,
              aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(group_by)),
              size = highlight_size,
              alpha = highlight_alpha
            )
        }
      }
    }
  }

  # Add color scale
  p <- p + scale_color_manual(
    values = colors[names(labels_tb)],
    labels = label_use,
    na.value = bg_color,
    guide = guide_legend(
      title.hjust = 0,
      order = 1,
      override.aes = list(size = 3, alpha = 1)
    )
  )

  # Add fill scale if needed (for hex)
  if (has_fill) {
    p <- p + scale_fill_manual(
      values = colors[names(labels_tb)],
      labels = label_use,
      na.value = bg_color,
      guide = guide_legend(
        title.hjust = 0,
        order = 1
      )
    )
  }

  # Add labels
  if (isTRUE(label)) {
    if (!is.null(facet_by)) {
      label_df <- stats::aggregate(
        data[, dims],
        by = list(data[[group_by]], data[[facet_by]]),
        FUN = stats::median
      )
      colnames(label_df)[2] <- facet_by
    } else {
      label_df <- stats::aggregate(data[, dims], by = list(data[[group_by]]), FUN = stats::median)
    }
    colnames(label_df)[1] <- ".label"
    label_df <- label_df[!is.na(label_df[, ".label"]), , drop = FALSE]

    if (!isTRUE(label_insitu)) {
      label_df[, ".label"] <- seq_len(nrow(label_df))
    }

    if (!requireNamespace("ggrepel", quietly = TRUE)) {
      stop("'ggrepel' package is required for labels. Install it with: install.packages('ggrepel')", call. = FALSE)
    }

    if (isTRUE(label_repel)) {
      p <- p +
        geom_point(
          data = label_df,
          mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
          color = label_pt_color, size = label_pt_size
        ) +
        ggrepel::geom_text_repel(
          data = label_df,
          aes(x = !!sym(dims[1]), y = !!sym(dims[2]), label = !!sym(".label")),
          point.size = label_pt_size, max.overlaps = 100, force = label_repulsion,
          color = label_fg, bg.color = label_bg, bg.r = label_bg_r,
          size = label_size, inherit.aes = FALSE
        )
    } else {
      p <- p +
        ggrepel::geom_text_repel(
          data = label_df,
          aes(x = !!sym(dims[1]), y = !!sym(dims[2]), label = !!sym(".label")),
          fontface = "bold", min.segment.length = 0, segment.color = label_segment_color,
          point.size = NA, max.overlaps = 100, force = 0,
          color = label_fg, bg.color = label_bg, bg.r = label_bg_r,
          size = label_size, inherit.aes = FALSE
        )
    }
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

#' Feature Dimension Reduction Plot
#'
#' @description
#' Visualizes continuous features (gene expression, etc.) on dimension reduction plots
#'
#' @inheritParams DimPlot
#' @param features Column name(s) of continuous features to plot.
#'   If multiple features provided, creates separate plots for each.
#' @param lower_quantile,upper_quantile Quantiles for color scale limits (0-1).
#' @param lower_cutoff,upper_cutoff Explicit cutoff values for color scale.
#' @param bg_cutoff Values below this threshold are set to NA (background color).
#' @param color_name Legend title for the color scale.
#'
#' @return A ggplot object or list/combined plots
#' @export
#' @examples
#' \dontrun{
#' # Single feature
#' FeatureDimPlot(data, features = "gene1", dims = 1:2)
#'
#' # Multiple features
#' FeatureDimPlot(data, features = c("gene1", "gene2"), dims = 1:2)
#'
#' # With splits
#' FeatureDimPlot(data, features = "gene1", split_by = "condition")
#' }
FeatureDimPlot <- function(
    data, dims = 1:2, features,
    lower_quantile = 0, upper_quantile = 0.99,
    lower_cutoff = NULL, upper_cutoff = NULL,
    bg_cutoff = NULL, color_name = "",
    split_by = NULL, split_by_sep = "_",
    pt_size = NULL, pt_alpha = 1, bg_color = "grey80",
    label = FALSE, label_size = 4,
    label_fg = "white", label_bg = "black", label_bg_r = 0.1,
    show_stat = !identical(theme, "theme_blank"),
    order = c("as-is", "reverse", "high-top", "low-top", "random"),
    highlight = NULL, highlight_alpha = 1, highlight_size = 1,
    highlight_color = "black", highlight_stroke = 0.8,
    add_density = FALSE, density_color = "grey80", density_filled = FALSE,
    density_filled_palette = "Greys", density_filled_palcolor = NULL,
    raster = NULL, raster_dpi = c(512, 512),
    hex = FALSE, hex_linewidth = 0.5, hex_count = FALSE,
    hex_bins = 50, hex_binwidth = NULL,
    facet_by = NULL, facet_scales = "fixed",
    facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    theme = "theme_ggforge_grid", theme_args = list(),
    palette = "Spectral", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = NULL, guides = NULL, design = NULL,
    ...) {
  # Validate common arguments
  validate_common_args(
    seed = seed,
    facet_by = facet_by,
    theme = theme,
    palette = palette,
    alpha = alpha,
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Process theme
  theme <- process_theme(theme)

  # Validate features exist
  features <- validate_columns(data, features, allow_multi = TRUE)

  # Check facet_by compatibility
  if (length(features) > 1 && !is.null(facet_by)) {
    stop("Cannot specify 'facet_by' with multiple features. The plot will be faceted by features.", call. = FALSE)
  }

  # If multiple features, split by them
  data_long <- NULL
  if (length(features) > 1) {
    # Transform data to long format
    data_long <- tidyr::pivot_longer(
      data,
      cols = features,
      names_to = ".feature",
      values_to = ".value"
    )
    data_long$.feature <- factor(data_long$.feature, levels = features)

    # Set facet_by to .feature
    facet_by <- ".feature"
    features <- ".value"
  }

  # Now call the atomic function with features
  params <- list(
    features = features,
    lower_quantile = lower_quantile,
    upper_quantile = upper_quantile,
    lower_cutoff = lower_cutoff,
    upper_cutoff = upper_cutoff,
    bg_cutoff = bg_cutoff,
    color_name = color_name,
    dims = dims,
    pt_size = pt_size,
    pt_alpha = pt_alpha,
    bg_color = bg_color,
    order = order,
    show_stat = show_stat,
    highlight = highlight,
    highlight_alpha = highlight_alpha,
    highlight_size = highlight_size,
    highlight_color = highlight_color,
    highlight_stroke = highlight_stroke,
    add_density = add_density,
    density_color = density_color,
    density_filled = density_filled,
    density_filled_palette = density_filled_palette,
    density_filled_palcolor = density_filled_palcolor,
    raster = raster,
    raster_dpi = raster_dpi,
    hex = hex,
    hex_linewidth = hex_linewidth,
    hex_count = hex_count,
    hex_bins = hex_bins,
    hex_binwidth = hex_binwidth,
    facet_by = facet_by,
    facet_scales = facet_scales,
    facet_nrow = facet_nrow,
    facet_ncol = facet_ncol,
    facet_byrow = facet_byrow,
    theme = theme,
    theme_args = theme_args,
    palette = palette,
    palcolor = palcolor,
    alpha = alpha,
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction,
    title = title,
    subtitle = subtitle,
    xlab = xlab,
    ylab = ylab
  )

  # Determine which data to use
  plot_data <- if (length(features) > 1 || !is.null(facet_by)) data_long else data

  # Validate and handle split_by
  split_by <- validate_columns(
    plot_data,
    split_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = split_by_sep
  )

  # Build plot
  build_plot(
    data = plot_data,
    atomic_fn = FeatureDimPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Feature Dimension Reduction Plot Atomic
#'
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_point scale_color_gradientn scale_fill_gradientn
#' @importFrom ggplot2 guide_colorbar labs theme
#' @importFrom rlang sym "%||%"
#' @importFrom scales rescale
#' @importFrom stats quantile
FeatureDimPlotAtomic <- function(
    data, dims, features,
    lower_quantile = 0, upper_quantile = 0.99,
    lower_cutoff = NULL, upper_cutoff = NULL,
    bg_cutoff = NULL, color_name = "",
    pt_size = NULL, pt_alpha = 1, bg_color = "grey80",
    order = c("as-is", "reverse", "high-top", "low-top", "random"),
    show_stat = TRUE,
    highlight = NULL, highlight_alpha = 1, highlight_size = 1,
    highlight_color = "black", highlight_stroke = 0.8,
    add_density = FALSE, density_color = "grey80", density_filled = FALSE,
    density_filled_palette = "Greys", density_filled_palcolor = NULL,
    raster = NULL, raster_dpi = c(512, 512),
    hex = FALSE, hex_linewidth = 0.5, hex_count = FALSE,
    hex_bins = 50, hex_binwidth = NULL,
    facet_by = NULL, facet_scales = "fixed",
    facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    theme = "theme_ggforge_grid", theme_args = list(),
    palette = "Spectral", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    ...) {
  # Get ggplot function (with gglogger support)
  ggplot <- get_ggplot()

  # Match order argument
  order <- match.arg(order)

  # Process dims
  if (is.numeric(dims)) {
    if (max(dims) > ncol(data)) {
      stop(sprintf("dims indices exceed number of columns (%d)", ncol(data)), call. = FALSE)
    }
    dims <- colnames(data)[dims]
  }

  if (length(dims) != 2) {
    stop("Only 2 dimensions are allowed.", call. = FALSE)
  }

  # Validate columns
  dims <- validate_columns(data, dims, allow_multi = TRUE)
  features <- validate_columns(data, features)
  facet_by <- validate_columns(data, facet_by, force_factor = TRUE)

  # Auto-calculate point size
  pt_size <- pt_size %||% min(3000 / nrow(data), 0.6)

  # Auto-determine raster
  raster <- raster %||% (nrow(data) > 1e5)

  # Ensure raster_dpi is length 2
  if (length(raster_dpi) == 1) {
    raster_dpi <- rep(raster_dpi, 2)
  }

  # Set default labels
  xlab <- xlab %||% dims[1]
  ylab <- ylab %||% dims[2]

  # Apply bg_cutoff
  if (!is.null(bg_cutoff)) {
    data[[features]][data[[features]] <= bg_cutoff] <- NA
  }

  # Calculate color scale limits
  if (all(is.na(data[[features]]))) {
    feat_colors_value <- rep(0, 100)
  } else {
    lower_cutoff <- lower_cutoff %||% stats::quantile(
      data[[features]][is.finite(data[[features]])],
      lower_quantile,
      na.rm = TRUE
    )
    upper_cutoff <- upper_cutoff %||% stats::quantile(
      data[[features]][is.finite(data[[features]])],
      upper_quantile,
      na.rm = TRUE
    )

    if (upper_cutoff == lower_cutoff) {
      if (upper_cutoff == 0) {
        upper_cutoff <- 1e-3
      } else {
        upper_cutoff <- upper_cutoff + upper_cutoff * 1e-3
      }
    }

    feat_colors_value <- seq(lower_cutoff, upper_cutoff, length.out = 100)
  }

  # Clip values to color range
  data[[features]][data[[features]] > max(feat_colors_value, na.rm = TRUE)] <- max(feat_colors_value, na.rm = TRUE)
  data[[features]][data[[features]] < min(feat_colors_value, na.rm = TRUE)] <- min(feat_colors_value, na.rm = TRUE)

  # Apply point ordering
  colorby <- features
  if (order == "reverse") {
    data <- data[nrow(data):1, , drop = FALSE]
  } else if (order == "high-top") {
    data <- dplyr::arrange(data, !!sym(colorby))
  } else if (order == "low-top") {
    data <- dplyr::arrange(data, dplyr::desc(!!sym(colorby)))
  } else if (order == "random") {
    data <- data[sample(nrow(data)), , drop = FALSE]
  }

  # Initialize plot
  p <- ggplot(data)

  # Add density
  if (isTRUE(add_density)) {
    if (isTRUE(density_filled)) {
      filled_color <- get_palette(
        palette = density_filled_palette,
        palcolor = density_filled_palcolor,
        type = "continuous"
      )
      p <- p +
        stat_density_2d(
          geom = "raster",
          aes(x = !!sym(dims[1]), y = !!sym(dims[2]), fill = after_stat(!!sym("density"))),
          contour = FALSE, inherit.aes = FALSE, show.legend = FALSE
        ) +
        scale_fill_gradientn(name = "Density", colours = filled_color)

      if (!requireNamespace("ggnewscale", quietly = TRUE)) {
        stop("'ggnewscale' package is required. Install it with: install.packages('ggnewscale')", call. = FALSE)
      }
      p <- p + ggnewscale::new_scale_fill()
    } else {
      p <- p + geom_density_2d(
        aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
        color = density_color, inherit.aes = FALSE, show.legend = FALSE
      )
    }
  }

  # Get axis limits
  x_min <- min(data[[dims[1]]], na.rm = TRUE)
  x_max <- max(data[[dims[1]]], na.rm = TRUE)
  y_min <- min(data[[dims[2]]], na.rm = TRUE)
  y_max <- max(data[[dims[2]]], na.rm = TRUE)

  # Add labels and scales
  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
    ggplot2::scale_x_continuous(limits = c(x_min, x_max)) +
    ggplot2::scale_y_continuous(limits = c(y_min, y_max))

  # Apply theme and automatic styling
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(aspect.ratio = aspect.ratio)

  # Apply data-driven styling (both dims are continuous)
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = dims[1],
    y_var = dims[2],
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Determine if we'll use fill scale
  has_fill <- FALSE

  # Add main points layer
  if (isTRUE(raster)) {
    if (!requireNamespace("scattermore", quietly = TRUE)) {
      stop("'scattermore' package is required. Install it with: install.packages('scattermore')", call. = FALSE)
    }
    # Add background layer for NA values
    p <- p + scattermore::geom_scattermore(
      mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
      color = bg_color,
      pointsize = ceiling(pt_size), alpha = pt_alpha, pixels = raster_dpi
    )
    # Add feature layer
    p <- p + scattermore::geom_scattermore(
      mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(colorby)),
      pointsize = ceiling(pt_size), alpha = pt_alpha, pixels = raster_dpi
    )
  } else if (isTRUE(hex)) {
    if (!requireNamespace("hexbin", quietly = TRUE)) {
      stop("'hexbin' package is required. Install it with: install.packages('hexbin')", call. = FALSE)
    }
    has_fill <- TRUE
    # Add NA values first
    data_na <- data[is.na(data[[features]]), , drop = FALSE]
    if (nrow(data_na) > 0) {
      p <- p + geom_hex(
        data = data_na,
        mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
        fill = bg_color, linewidth = hex_linewidth, bins = hex_bins,
        binwidth = hex_binwidth, alpha = pt_alpha / 2
      )
    }
    # Add feature hex
    p <- p + stat_summary_hex(
      data = data[!is.na(data[[features]]), , drop = FALSE],
      mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), z = !!sym(colorby)),
      linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth, alpha = pt_alpha
    )
  } else {
    # Regular points
    p <- p + geom_point(
      mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(colorby)),
      size = pt_size, alpha = pt_alpha
    )
  }

  # Add highlighting
  if (!is.null(highlight) && !isTRUE(hex)) {
    if (isTRUE(highlight)) {
      hi_df <- data
    } else if (length(highlight) == 1 && is.character(highlight)) {
      hi_df <- dplyr::filter(data, eval(parse(text = highlight)))
    } else {
      all_inst <- if (is.numeric(highlight)) 1:nrow(data) else rownames(data)
      if (any(highlight %in% all_inst)) {
        hi_df <- data[intersect(highlight, all_inst), , drop = FALSE]
      } else {
        hi_df <- data[0, , drop = FALSE]
      }
    }

    if (nrow(hi_df) > 0) {
      if (isTRUE(raster)) {
        p <- p +
          scattermore::geom_scattermore(
            data = hi_df,
            aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
            color = highlight_color,
            pointsize = floor(highlight_size) + highlight_stroke,
            alpha = highlight_alpha, pixels = raster_dpi
          ) +
          scattermore::geom_scattermore(
            data = hi_df,
            aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(colorby)),
            pointsize = floor(highlight_size),
            alpha = highlight_alpha, pixels = raster_dpi
          )
      } else {
        p <- p +
          geom_point(
            data = hi_df,
            aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
            color = highlight_color,
            size = highlight_size + highlight_stroke,
            alpha = highlight_alpha
          ) +
          geom_point(
            data = hi_df,
            aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(colorby)),
            size = highlight_size,
            alpha = highlight_alpha
          )
      }
    }
  }

  # Add color scale for continuous values
  feat_colors <- get_palette(
    palette = palette,
    palcolor = palcolor,
    type = "continuous"
  )

  p <- p + scale_color_gradientn(
    name = color_name,
    colors = feat_colors,
    values = scales::rescale(feat_colors_value),
    limits = range(feat_colors_value),
    na.value = bg_color,
    guide = guide_colorbar(
      frame.colour = "black",
      ticks.colour = "black",
      frame.linewidth = 0.3,
      ticks.linewidth = 0.3,
      title.hjust = 0
    )
  )

  if (has_fill) {
    p <- p + scale_fill_gradientn(
      name = color_name,
      colors = feat_colors,
      values = scales::rescale(feat_colors_value),
      limits = range(feat_colors_value),
      na.value = bg_color,
      guide = guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black",
        frame.linewidth = 0.3,
        ticks.linewidth = 0.3,
        title.hjust = 0
      )
    )
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
