#' Volcano Plot
#'
#' @description
#' A volcano plot is a type of scatter plot that shows statistical significance
#' (usually on the y-axis) versus magnitude of change (usually on the x-axis).
#'
#' @inheritParams parameters
#' @param x A character string of the column name to plot on the x-axis (e.g., log fold change).
#' @param y A character string of the column name to plot on the y-axis (e.g., p-value).
#' @param ytrans A function to transform the y-axis values. Default is `-log10(n)`.
#' @param color_by A character vector of column names to color the points by.
#'   If NULL, the points will be filled by the x and y cutoff value.
#' @param color_name A character string to name the legend of color.
#' @param flip_negatives A logical value to flip the y-axis for negative x values.
#' @param trim A numeric vector of length 2 to trim the x-axis values.
#'   The values must be in the range from 0 to 1, which works as quantile to trim the x-axis values.
#'   For example, c(0.01, 0.99) will trim the 1% and 99% quantile of the x-axis values.
#' @param xlim A numeric vector of length 2 to set the x-axis limits.
#' @param x_cutoff A numeric value to set the x-axis cutoff.
#'   Both negative and positive of this value will be used.
#' @param y_cutoff A numeric value to set the y-axis cutoff.
#'   Note that the y-axis cutoff will be transformed by `ytrans`.
#' @param x_cutoff_name A character string to name the x-axis cutoff.
#'   If "none", the legend for the x-axis cutoff will not be shown.
#' @param y_cutoff_name A character string to name the y-axis cutoff.
#'   If "none", the legend for the y-axis cutoff will not be shown.
#' @param x_cutoff_color A character string to color the x-axis cutoff line.
#' @param y_cutoff_color A character string to color the y-axis cutoff line.
#' @param x_cutoff_linetype A character string to set the x-axis cutoff line type.
#' @param y_cutoff_linetype A character string to set the y-axis cutoff line type.
#' @param x_cutoff_linewidth A numeric value to set the x-axis cutoff line size.
#' @param y_cutoff_linewidth A numeric value to set the y-axis cutoff line size.
#' @param pt_size A numeric value to set the point size.
#' @param pt_alpha A numeric value to set the point transparency.
#' @param nlabel A numeric value to set the number of labels to show.
#'   The points will be ordered by the distance to the origin. Top `nlabel` points will be labeled.
#' @param labels A character vector of row names or indexes to label the points.
#' @param label_by A character string of column name to use as labels.
#'   If NULL, the row names will be used.
#' @param label_size A numeric value to set the label size.
#' @param label_fg A character string to set the label color.
#' @param label_bg A character string to set the label background color.
#' @param label_bg_r A numeric value specifying the radius of the background of the label.
#' @param highlight A character vector of row names or indexes to highlight the points.
#' @param highlight_color A character string to set the highlight color.
#' @param highlight_size A numeric value to set the highlight size.
#' @param highlight_alpha A numeric value to set the highlight transparency.
#' @param highlight_stroke A numeric value to set the highlight stroke size.
#'
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' \donttest{
#' set.seed(8525)
#' # Obtained by Seurat::FindMarkers for the first cluster of pbmc_small
#' data <- data.frame(
#'   avg_log2FC = c(
#'     -3.69, -4.10, -2.68, -3.51, -3.09, -2.52, -3.53, -3.35, -2.82, -2.71,
#'     3.67, 4.79, 10.14, 5.36, 4.56, 4.62, 3.31, 4.72, 3.01, 3.86
#'   ),
#'   p_val_adj = c(
#'     3.82e-09, 1.52e-07, 1.79e-07, 4.68e-07, 4.83e-07, 6.26e-07, 2.61e-06,
#'     1.33e-05, 1.79e-05, 3.71e-05, 8.93e-04, 9.61e-04, 1.47e-03, 4.35e-03,
#'     4.85e-03, 5.12e-03, 1.90e-02, 2.13e-02, 3.80e-02, 6.72e-02
#'   ),
#'   gene = c(
#'     "HLA-DPB1", "LYZ", "HLA-DRA", "TYMP", "HLA-DPA1", "HLA-DRB1", "CST3",
#'     "HLA-DQB1", "HLA-DRB5", "LST1", "CCL5", "LCK", "MS4A6A", "CD3D", "CD7",
#'     "CD3E", "CTSW", "GZMM", "GZMA", "IL32"
#'   ),
#'   group = sample(LETTERS[1:2], 20, replace = TRUE)
#' )
#'
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "-log10(0.05)")
#' VolcanoPlot(data,
#'   x = "avg_log2FC", y = "p_val_adj",
#'   label_by = "gene", y_cutoff_name = "-log10(0.05)"
#' )
#' VolcanoPlot(data,
#'   x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
#'   flip_negatives = TRUE, label_by = "gene"
#' )
#' VolcanoPlot(data,
#'   x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
#'   flip_negatives = TRUE, facet_by = "group", label_by = "gene"
#' )
#' VolcanoPlot(data,
#'   x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
#'   flip_negatives = TRUE, split_by = "group", label_by = "gene"
#' )
#' }
VolcanoPlot <- function(
    data, x, y,
    ytrans = function(n) -log10(n),
    color_by = NULL,
    color_name = NULL,
    flip_negatives = FALSE,
    x_cutoff = NULL,
    y_cutoff = 0.05,
    trim = c(0, 1),
    xlim = NULL,
    x_cutoff_name = NULL,
    y_cutoff_name = NULL,
    x_cutoff_color = "red2",
    y_cutoff_color = "blue2",
    x_cutoff_linetype = "dashed",
    y_cutoff_linetype = "dashed",
    x_cutoff_linewidth = 0.5,
    y_cutoff_linewidth = 0.5,
    pt_size = 2,
    pt_alpha = 0.5,
    nlabel = 5,
    labels = NULL,
    label_by = NULL,
    label_size = 3,
    label_fg = "black",
    label_bg = "white",
    label_bg_r = 0.1,
    highlight = NULL,
    highlight_color = "red",
    highlight_size = 2,
    highlight_alpha = 1,
    highlight_stroke = 0.5,
    split_by = NULL,
    split_by_sep = "_",
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
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
  # Validate trim parameter
  stopifnot(
    "[VolcanoPlot] 'trim' must be a numeric vector of length 2 and both values must be in the range [0, 1]." =
      length(trim) == 2 && all(trim >= 0 & trim <= 1)
  )
  stopifnot("[VolcanoPlot] 'xlim' must be a numeric vector of length 2." = is.null(xlim) || length(xlim) == 2)

  # Validate common arguments
  validate_common_args(
    seed = seed,
    facet_by = facet_by,
    split_by = split_by,
    theme = theme,
    palette = palette,
    alpha = pt_alpha,
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
    atomic_fn = VolcanoPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Volcano Plot Atomic
#'
#' @description
#' Creates a single volcano plot without splitting
#'
#' @inheritParams VolcanoPlot
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_point geom_vline geom_hline scale_color_manual
#' @importFrom ggplot2 scale_color_gradientn guide_colorbar guide_legend guide_none
#' @importFrom ggplot2 labs theme coord_cartesian scale_y_continuous xlim
#' @importFrom dplyr mutate case_when group_by arrange desc row_number ungroup filter distinct
#' @importFrom rlang sym syms ":=" "%||%"
#' @importFrom tidyr expand_grid
#' @importFrom scales number percent rescale
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggnewscale new_scale_color
VolcanoPlotAtomic <- function(
    data, x, y,
    ytrans = function(n) -log10(n),
    color_by = NULL,
    color_name = NULL,
    flip_negatives = FALSE,
    x_cutoff = NULL,
    y_cutoff = 0.05,
    trim = c(0, 1),
    xlim = NULL,
    x_cutoff_name = NULL,
    y_cutoff_name = NULL,
    x_cutoff_color = "red2",
    y_cutoff_color = "blue2",
    x_cutoff_linetype = "dashed",
    y_cutoff_linetype = "dashed",
    x_cutoff_linewidth = 0.5,
    y_cutoff_linewidth = 0.5,
    pt_size = 2,
    pt_alpha = 0.5,
    nlabel = 5,
    labels = NULL,
    label_by = NULL,
    label_size = 3,
    label_fg = "black",
    label_bg = "white",
    label_bg_r = 0.1,
    highlight = NULL,
    highlight_color = "red",
    highlight_size = 2,
    highlight_alpha = 1,
    highlight_stroke = 0.5,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    seed = 8525,
    ...) {
  # Get ggplot function (support gglogger)
  ggplot <- get_ggplot()

  # Validate and process columns
  x <- validate_columns(data, x)
  y <- validate_columns(data, y)
  color_by <- validate_columns(data, color_by)
  facet_by <- validate_columns(
    data, facet_by,
    force_factor = TRUE,
    allow_multi = TRUE
  )
  label_by <- validate_columns(data, label_by)

  # Normalize legend position
  legend.position <- normalize_legend_position(
    legend.position,
    has_group = !is.null(color_by)
  )

  # Transform y-axis
  data[[y]] <- ytrans(data[[y]])

  # Set default cutoff values and transform y_cutoff
  x_cutoff <- x_cutoff %||% 0
  y_cutoff <- ytrans(y_cutoff)

  # Create category based on cutoffs
  if (!is.null(y_cutoff)) {
    data <- data %>%
      dplyr::mutate(
        .category = factor(
          dplyr::case_when(
            !!rlang::sym(x) > x_cutoff & !!rlang::sym(y) > y_cutoff ~ "sig_pos_x",
            !!rlang::sym(x) < -x_cutoff & !!rlang::sym(y) > y_cutoff ~ "sig_neg_x",
            TRUE ~ "insig"
          ),
          levels = c("sig_neg_x", "insig", "sig_pos_x")
        )
      )
  } else {
    data <- data %>%
      dplyr::mutate(
        .category = factor(
          dplyr::case_when(
            !!rlang::sym(x) > x_cutoff ~ "sig_pos_x",
            !!rlang::sym(x) < -x_cutoff ~ "sig_neg_x",
            TRUE ~ "insig"
          ),
          levels = c("sig_neg_x", "insig", "sig_pos_x")
        )
      )
  }

  # Determine color type
  if (is.null(color_by)) {
    color_by <- ".category"
    color_type <- "discrete"
  } else if (is.character(data[[color_by]]) || is.factor(data[[color_by]])) {
    color_type <- "discrete"
  } else {
    color_type <- "continuous"
  }

  # Flip negatives if requested
  if (flip_negatives) {
    data[data[[x]] < 0, y] <- -data[data[[x]] < 0, y]
  }

  # Set up label column (use rownames if label_by is not specified)
  data$.label <- if (is.null(label_by)) base::rownames(data) else data[[label_by]]
  data$.show_label <- FALSE

  # Trim outliers based on quantile values
  trim <- base::sort(trim)
  x_upper <- stats::quantile(data[[x]][base::is.finite(data[[x]])], c(trim[2], 1))
  x_lower <- stats::quantile(data[[x]][base::is.finite(data[[x]])], c(trim[1], 0))
  x_upper <- base::ifelse(x_upper[1] > 0, x_upper[1], x_upper[2])
  x_lower <- base::ifelse(x_lower[1] < 0, x_lower[1], x_lower[2])

  # Ensure symmetric range if data spans both positive and negative values
  if (x_upper > 0 & x_lower < 0) {
    value_range <- base::min(base::abs(c(x_upper, x_lower)), na.rm = TRUE)
    x_upper <- value_range
    x_lower <- -value_range
  }

  # Mark outliers and clip values to range
  data$.outlier <- data[[x]] > x_upper | data[[x]] < x_lower
  data[[x]][data[[x]] > x_upper] <- x_upper
  data[[x]][data[[x]] < x_lower] <- x_lower

  # Determine which points to label
  if (!is.null(labels)) {
    data[labels, ".show_label"] <- TRUE
  } else {
    # Calculate the distance to the origin for each point
    data$.distance <- sqrt(data[[x]]^2 + data[[y]]^2)
    if (!is.null(facet_by)) {
      data <- data %>%
        dplyr::group_by(!!!rlang::syms(facet_by), base::sign(data[[x]])) %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym(".distance"))) %>%
        dplyr::mutate(.show_label = dplyr::row_number() <= nlabel) %>%
        dplyr::ungroup()
    } else {
      data <- data %>%
        dplyr::group_by(base::sign(data[[x]])) %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym(".distance"))) %>%
        dplyr::mutate(.show_label = dplyr::row_number() <= nlabel) %>%
        dplyr::ungroup()
    }
  }

  # Only show labels for significant points
  data <- data %>%
    dplyr::mutate(.show_label = !!rlang::sym(".show_label") & !!rlang::sym(".category") != "insig") %>%
    base::as.data.frame()

  # Split data for positive and negative values (for label nudging)
  pos_data <- data[data[[x]] >= 0, , drop = FALSE]
  neg_data <- data[data[[x]] < 0, , drop = FALSE]
  outlier_data <- data[data$.outlier, , drop = FALSE]

  # Calculate nudge values for labels to avoid overlap
  pos_x_nudge <- -base::diff(base::range(pos_data[[x]])) * 0.05
  neg_x_nudge <- base::diff(base::range(neg_data[[x]])) * 0.05
  jitter <- ggplot2::position_jitter(width = 0.2, height = 0.2, seed = seed)

  # Build plot
  p <- ggplot(mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), color = !!rlang::sym(color_by))) +
    ggplot2::geom_point(data = pos_data, size = pt_size, alpha = pt_alpha) +
    ggplot2::geom_point(data = neg_data, size = pt_size, alpha = pt_alpha) +
    ggplot2::geom_point(data = outlier_data, size = pt_size, alpha = pt_alpha, position = jitter)

  # Add color scale (discrete or continuous)
  if (color_type == "discrete") {
    colors <- get_palette(base::levels(data[[color_by]]), palette = palette, palcolor = palcolor)
    if (base::is.null(palcolor)) {
      colors["insig"] <- "grey"
    }
    p <- p + ggplot2::scale_color_manual(values = colors, guide = "none")
  } else {
    p <- p + ggplot2::scale_color_gradientn(
      colors = get_palette(palette = palette, palcolor = palcolor),
      values = scales::rescale(base::unique(c(
        base::min(c(base::unlist(data[[color_by]]), 0), na.rm = TRUE), 0,
        base::max(base::unlist(data[[color_by]]), na.rm = TRUE)
      ))),
      guide = ggplot2::guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black",
        frame.linewidth = 0.3,
        ticks.linewidth = 0.3,
        title.hjust = 0,
        order = 1
      )
    )
  }

  # Add highlight points
  if (!is.null(highlight)) {
    p <- p +
      ggplot2::geom_point(
        data = data[highlight, , drop = FALSE] %>% dplyr::filter(!!rlang::sym(".outlier") == FALSE),
        color = highlight_color, size = highlight_size, alpha = highlight_alpha, stroke = highlight_stroke
      ) +
      ggplot2::geom_point(
        data = data[highlight, , drop = FALSE] %>% dplyr::filter(!!rlang::sym(".outlier")),
        color = highlight_color, size = highlight_size, alpha = highlight_alpha,
        stroke = highlight_stroke, position = jitter
      )
  }

  # Add x_cutoff lines
  if (!is.null(x_cutoff) && x_cutoff != 0) {
    if (identical(x_cutoff_name, "none")) {
      guide <- ggplot2::guide_none()
    } else {
      guide <- ggplot2::guide_legend(
        override.aes = list(alpha = 0.8, size = 5), order = 2,
        theme = ggplot2::theme(legend.margin = ggplot2::margin(0, 0, -10, 4.5))
      )
    }
    vline_df <- data.frame(xintercept = c(-x_cutoff, x_cutoff))
    if (!is.null(facet_by)) {
      vline_df <- tidyr::expand_grid(vline_df, data[, facet_by, drop = FALSE] %>% dplyr::distinct())
    }
    p <- p +
      ggnewscale::new_scale_color() +
      ggplot2::geom_vline(
        data = vline_df,
        mapping = ggplot2::aes(
          xintercept = !!rlang::sym("xintercept"),
          color = x_cutoff_name %||% paste0(x, " = +/-", scales::number(x_cutoff, accuracy = 0.01))
        ),
        alpha = 0.4, linetype = x_cutoff_linetype, linewidth = x_cutoff_linewidth
      ) +
      ggplot2::scale_color_manual(name = NULL, values = x_cutoff_color, guide = guide)
  }

  # Add y_cutoff lines
  if (!is.null(y_cutoff)) {
    if (isTRUE(flip_negatives)) {
      yintercept <- c(-y_cutoff, y_cutoff)
    } else {
      yintercept <- y_cutoff
    }
    if (identical(y_cutoff_name, "none")) {
      guide <- ggplot2::guide_none()
    } else {
      guide <- ggplot2::guide_legend(override.aes = list(alpha = 0.8, size = 5), order = 3)
    }
    hline_df <- data.frame(yintercept = yintercept)
    if (!is.null(facet_by)) {
      hline_df <- tidyr::expand_grid(hline_df, data[, facet_by, drop = FALSE] %>% dplyr::distinct())
    }
    p <- p +
      ggnewscale::new_scale_color() +
      ggplot2::geom_hline(
        data = hline_df,
        mapping = ggplot2::aes(
          yintercept = !!rlang::sym("yintercept"),
          color = y_cutoff_name %||% paste0(ylab %||% y, " = ", scales::number(y_cutoff, accuracy = 0.01))
        ),
        alpha = 0.4, linetype = y_cutoff_linetype, linewidth = y_cutoff_linewidth
      ) +
      ggplot2::scale_color_manual(name = NULL, values = y_cutoff_color, guide = guide)
  }

  # Add flip negative handling
  if (isTRUE(flip_negatives)) {
    p <- p +
      ggplot2::geom_hline(yintercept = 0, color = "black", linetype = 1) +
      ggplot2::scale_y_continuous(labels = abs)
  }

  # Set xlim
  if (!is.null(xlim)) {
    p <- p + ggplot2::xlim(xlim)
  }

  # Add reference line and labels
  p <- p +
    ggplot2::geom_vline(xintercept = 0, color = "grey80", linetype = 2) +
    ggrepel::geom_text_repel(
      data = pos_data[pos_data$.show_label, , drop = FALSE],
      mapping = ggplot2::aes(label = !!rlang::sym(".label")),
      nudge_x = pos_x_nudge, color = label_fg,
      bg.color = label_bg, bg.r = label_bg_r, size = label_size,
      min.segment.length = 0, segment.color = "grey40", max.overlaps = 100
    ) +
    ggrepel::geom_text_repel(
      data = neg_data[neg_data$.show_label, , drop = FALSE],
      mapping = ggplot2::aes(label = !!rlang::sym(".label")),
      nudge_x = neg_x_nudge, color = label_fg,
      bg.color = label_bg, bg.r = label_bg_r, size = label_size,
      min.segment.length = 0, segment.color = "grey40", max.overlaps = 100
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
    ggplot2::coord_cartesian(clip = "off")

  # Apply theme and automatic styling
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(aspect.ratio = aspect.ratio)

  # Apply data-driven styling (both x and y are continuous)
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

  # Add faceting
  if (!is.null(facet_by)) {
    p <- add_facets(
      p, facet_by, facet_scales,
      facet_nrow, facet_ncol, facet_byrow
    )
  }

  # Set dimensions as attributes
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

  attr(p, "height") <- height
  attr(p, "width") <- width

  return(p)
}
