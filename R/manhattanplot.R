#' Manhattan Plot
#'
#' @description
#' Creates a Manhattan plot for visualizing genome-wide association study (GWAS) results.
#' This function is adapted from `ggmanh::manhattan_plot()` with several enhancements:
#'
#' * Parameter names use underscores instead of dots for consistency
#' * `chr.colname`, `pos.colname`, `pval.colname` and `label.colname` are renamed to
#'   `chr_by`, `pos_by`, `pval_by` and `label_by`
#' * The `chromosome` and `chr.order` arguments are merged into `chromosomes`
#' * The `highlight.colname` argument is replaced with `highlight`, which can be a vector of indices
#'   or a character expression to select variants, instead of a column name
#' * Point styling controlled by `pt_*` arguments (size, color, alpha, shape)
#' * Label styling controlled by `label_*` arguments
#' * Highlight styling controlled by `highlight_*` arguments
#' * Flexible p-value transformation via `pval_transform` function
#' * Improved significance threshold handling
#'
#' @inheritParams parameters
#' @param data A data frame or `GenomicRanges::GRanges` containing the data to be plotted.
#' @param chr_by Column name for chromosome (default: "chr").
#' @param pos_by Column name for position (default: "pos").
#' @param pval_by Column name for p-value (default: "pval").
#' @param label_by Column name for the variants to be labeled (default: NULL).
#'   Only the variants with non-empty values in this column will be labeled.
#' @param chromosomes A vector of chromosomes to be plotted (default: NULL).
#'   If NULL, all chromosomes will be plotted.
#'   Can be used to select chromosomes to be plotted or to set the order of the chromosomes.
#' @param pt_size A numeric value to specify the size of the points in the plot.
#' @param pt_color A character string to specify the color of the points in the plot.
#'   By default, the color of the points will be controlled by `palette` or `palcolor` arguments.
#'   This is useful to color the background points when `highlight` and `highlight_color`
#'   are specified.
#' @param pt_alpha A numeric value to specify the transparency of the points in the plot.
#' @param pt_shape A numeric value to specify the shape of the points in the plot.
#' @param label_size A numeric value to specify the size of the labels in the plot.
#' @param label_fg A character string to specify the color of the labels in the plot.
#'   If NULL, the color of the labels will be the same as the points.
#' @param highlight Either a vector of indices or a character expression to select
#'   the variants to be highlighted (default: NULL).
#'   If NULL, no variants will be highlighted.
#' @param highlight_color A character string to specify the color of the highlighted points.
#' @param highlight_size A numeric value to specify the size of the highlighted points.
#' @param highlight_alpha A numeric value to specify the transparency of the highlighted points.
#' @param highlight_shape A numeric value to specify the shape of the highlighted points.
#' @param preserve_position If TRUE, the width of each chromosome reflects the number of variants
#'   and the position of each variant is correctly scaled.
#'   If FALSE, the width of each chromosome is equal and the variants are equally spaced.
#' @param chr_gap_scaling A numeric value to specify the scaling of the gap between chromosomes.
#'   It is used to adjust the gap between chromosomes in the plot.
#' @param pval_transform A function to transform the p-values (default: "-log10").
#'   If it is a character, it will be evaluated as a function.
#' @param signif A vector of significance thresholds (default: c(5e-08, 1e-05)).
#' @param signif_color A character vector of equal length as signif.
#'   It contains colors for the lines drawn at signif.
#'   If NULL, the smallest value is colored black while others are grey.
#' @param signif_rel_pos A numeric between 0.1 and 0.9. If the plot is rescaled,
#'   where should the significance threshold be positioned?
#' @param signif_label A logical value indicating whether to label the significance thresholds (default: TRUE).
#' @param signif_label_size A numeric value to specify the size of the significance labels.
#' @param signif_label_pos A character string specifying the position of the significance labels.
#'   It can be either "left" or "right" (default: "left").
#' @param thin A logical value indicating whether to thin the data (default: NULL).
#'   Defaults to TRUE when `chromosomes` is specified and the length of
#'   it is less than the number of chromosomes in the data. Defaults to FALSE otherwise.
#' @param thin_n Number of max points per horizontal partitions of the plot. Defaults to 1000.
#' @param thin_bins Number of bins to partition the data. Defaults to 200.
#' @param rescale A logical value indicating whether to rescale the plot (default: TRUE).
#' @param rescale_ratio_threshold A numeric value to specify the ratio threshold for rescaling.
#' @param palreverse A logical value indicating whether to reverse the palette for chromosomes (default: FALSE).
#'
#' @return A ggplot object or wrap_plots object or a list of ggplot objects.
#'   If no `split_by` is provided, a single plot (ggplot object) will be returned.
#'   If 'combine' is TRUE, a wrap_plots object will be returned.
#'   If 'combine' is FALSE, a list of ggplot objects will be returned.
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' \donttest{
#' set.seed(1000)
#'
#' nsim <- 50000
#'
#' simdata <- data.frame(
#'   "chromosome" = sample(c(1:22, "X"), size = nsim, replace = TRUE),
#'   "position" = sample(1:100000000, size = nsim),
#'   "P.value" = rbeta(nsim, shape1 = 5, shape2 = 1)^7,
#'   "cohort" = sample(c("A", "B"), size = nsim, replace = TRUE)
#' )
#' simdata$chromosome <- factor(simdata$chromosome, c(1:22, "X"))
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#'   ManhattanPlot(
#'     simdata,
#'     pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'     title = "Simulated P.Values", ylab = "P"
#'   )
#' }
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#'   # split_by
#'   ManhattanPlot(
#'     simdata,
#'     pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'     title = "Simulated P.Values", ylab = "P", split_by = "cohort", ncol = 1
#'   )
#' }
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#'   # Customized p-value transformation and significance threshold line colors
#'   ManhattanPlot(
#'     simdata,
#'     pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'     title = "Simulated -Log2 P.Values", ylab = "-log2(P)", pval_transform = "-log2",
#'     signif_color = c("red", "blue")
#'   )
#' }
#' }
ManhattanPlot <- function(
    data, chr_by, pos_by, pval_by,
    split_by = NULL, split_by_sep = "_",
    label_by = NULL,
    chromosomes = NULL,
    pt_size = 0.75,
    pt_color = NULL,
    pt_alpha = alpha,
    pt_shape = 19,
    label_size = 3,
    label_fg = NULL,
    highlight = NULL,
    highlight_color = NULL,
    highlight_size = 1.5,
    highlight_alpha = 1,
    highlight_shape = 19,
    preserve_position = TRUE,
    chr_gap_scaling = 1,
    pval_transform = "-log10",
    signif = c(5e-08, 1e-05),
    signif_color = NULL,
    signif_rel_pos = 0.2,
    signif_label = TRUE,
    signif_label_size = 3.5,
    signif_label_pos = c("left", "right"),
    thin = NULL,
    thin_n = 1000,
    thin_bins = 200,
    rescale = TRUE,
    rescale_ratio_threshold = 5,
    palette = "Dark2",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    theme = "theme_ggforge",
    theme_args = list(),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = expression("\u002d" * log[10](p)),
    seed = 8525,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    axes = NULL,
    axis_titles = NULL,
    guides = NULL,
    facet_by = NULL,
    design = NULL,
    ...) {
  # Validate common arguments
  validate_common_args(seed = seed)

  # facet_by is not supported for Manhattan plots
  if (!is.null(facet_by)) {
    warning("[ManhattanPlot] 'facet_by' is not supported, using 'split_by' instead.", call. = FALSE)
  }

  # Process theme
  theme <- process_theme(theme)

  # Handle data type (data.frame or GRanges)
  if (is.data.frame(data)) {
    split_by <- validate_columns(
      data, split_by,
      force_factor = TRUE,
      allow_multi = TRUE,
      concat_multi = TRUE,
      concat_sep = split_by_sep
    )
  } else if (inherits(data, "GRanges")) {
    metadata <- data@elementMetadata
    split_by <- validate_columns(
      metadata, split_by,
      force_factor = TRUE,
      allow_multi = TRUE,
      concat_multi = TRUE,
      concat_sep = split_by_sep
    )
    data@elementMetadata <- metadata
  } else {
    stop("[ManhattanPlot] 'data' must be a data frame or a GRanges object.", call. = FALSE)
  }

  # Split data if requested
  if (!is.null(split_by)) {
    if (is.data.frame(data)) {
      datas <- split(data, data[[split_by]])
      datas <- datas[levels(data[[split_by]])]
    } else {
      datas <- split(data, data@elementMetadata[[split_by]])
      datas <- datas[levels(data@elementMetadata[[split_by]])]
    }
  } else {
    datas <- list(data)
    names(datas) <- "..."
  }

  # Normalize palette and palcolor for splits
  palette <- check_palette(palette, names(datas))
  palcolor <- check_palcolor(palcolor, names(datas))

  # Build plots for each split
  plots <- lapply(
    names(datas), function(nm) {
      default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
      if (is.function(title)) {
        plot_title <- title(default_title)
      } else {
        plot_title <- title %||% default_title
      }

      ManhattanPlotAtomic(
        data = datas[[nm]],
        chr_by = chr_by,
        pos_by = pos_by,
        pval_by = pval_by,
        label_by = label_by,
        chromosomes = chromosomes,
        pt_size = pt_size,
        pt_color = pt_color,
        pt_alpha = pt_alpha,
        pt_shape = pt_shape,
        label_size = label_size,
        label_fg = label_fg,
        highlight = highlight,
        highlight_color = highlight_color,
        highlight_size = highlight_size,
        highlight_alpha = highlight_alpha,
        highlight_shape = highlight_shape,
        preserve_position = preserve_position,
        chr_gap_scaling = chr_gap_scaling,
        pval_transform = pval_transform,
        signif = signif,
        signif_color = signif_color,
        signif_rel_pos = signif_rel_pos,
        signif_label = signif_label,
        signif_label_size = signif_label_size,
        signif_label_pos = signif_label_pos,
        thin = thin,
        thin_n = thin_n,
        thin_bins = thin_bins,
        rescale = rescale,
        rescale_ratio_threshold = rescale_ratio_threshold,
        palette = palette[[nm]],
        palcolor = palcolor[[nm]],
        palreverse = palreverse,
        alpha = alpha,
        theme_args = theme_args,
        theme = theme,
        title = plot_title,
        subtitle = subtitle,
        xlab = xlab,
        ylab = ylab
      )
    }
  )

  # Combine plots
  combine_plots(
    plots,
    combine = combine,
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    axes = axes,
    axis_titles = axis_titles,
    guides = guides,
    design = design
  )
}

#' Manhattan Plot Atomic
#'
#' @description
#' Plot a Manhattan plot for atomic data (without splitting).
#' This is an internal function that creates the actual plot.
#'
#' @inheritParams ManhattanPlot
#' @keywords internal
#' @importFrom rlang sym "%||%"
#' @importFrom ggplot2 waiver aes geom_point scale_y_continuous scale_color_manual annotate
#' @importFrom ggplot2 scale_x_continuous geom_hline element_line element_blank coord_cartesian geom_text
#' @importFrom ggrepel geom_label_repel
#' @importFrom utils getFromNamespace
#' @return A ggplot object.
ManhattanPlotAtomic <- function(
    data, chr_by, pos_by, pval_by,
    label_by = NULL,
    chromosomes = NULL,
    pt_size = 0.75,
    pt_color = NULL,
    pt_alpha = alpha,
    pt_shape = 19,
    label_size = 3,
    label_fg = NULL,
    highlight = NULL,
    highlight_color = NULL,
    highlight_size = 1.5,
    highlight_alpha = 1,
    highlight_shape = 19,
    preserve_position = TRUE,
    chr_gap_scaling = 1,
    pval_transform = "-log10",
    signif = c(5e-08, 1e-05),
    signif_color = NULL,
    signif_rel_pos = 0.2,
    signif_label = TRUE,
    signif_label_size = 3.5,
    signif_label_pos = c("left", "right"),
    thin = NULL,
    thin_n = 1000,
    thin_bins = 200,
    rescale = TRUE,
    rescale_ratio_threshold = 5,
    palette = "Dark2",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    theme = "theme_ggforge",
    theme_args = list(),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = expression("\u002d" * log[10](p)),
    ...) {
  # Get ggplot function (support gglogger)
  ggplot <- get_ggplot()

  # Match signif_label_pos argument
  signif_label_pos <- match.arg(signif_label_pos)

  # Preprocess data using ggmanh
  mpdata <- suppressWarnings({
    ggmanh::manhattan_data_preprocess(
      x = data,
      chromosome = if (length(chromosomes) == 1) chromosomes else NULL,
      signif = signif,
      pval.colname = pval_by,
      chr.colname = chr_by,
      pos.colname = pos_by,
      chr.order = chromosomes,
      signif.col = signif_color,
      preserve.position = preserve_position,
      thin = thin,
      thin.n = thin_n,
      thin.bins = thin_bins,
      pval.log.transform = TRUE,
      chr.gap.scaling = chr_gap_scaling
    )
  })

  # Validate label column
  label_by <- validate_columns(mpdata$data, label_by, force_factor = TRUE)

  # Set up chromosome colors
  if ((!is.null(highlight) && !is.null(highlight_color)) || !is.null(pt_color)) {
    pt_color <- pt_color %||% "grey80"
    mpdata$chr.col <- rep(pt_color, length(levels(mpdata$data[[chr_by]])))
    names(mpdata$chr.col) <- levels(mpdata$data[[chr_by]])
  } else {
    mpdata$chr.col <- get_palette(
      levels(mpdata$data[[chr_by]]),
      palette = palette,
      palcolor = palcolor,
      reverse = palreverse,
      alpha = pt_alpha
    )
  }

  # Process p-value transformation
  if (is.character(pval_transform) && startsWith(pval_transform, "-")) {
    pval_transform <- sub("^-", "", pval_transform)
    pval_transform_fn <- eval(parse(text = pval_transform))
    pval_transform <- function(x) -pval_transform_fn(x)
  }
  if (is.character(pval_transform)) {
    pval_transform <- eval(parse(text = pval_transform))
  }
  stopifnot("[ManhattanPlot] 'pval_transform' must be a function" = is.function(pval_transform))

  mpdata$data$log10pval <- pval_transform(mpdata$data[[pval_by]])
  mpdata$pval.colname <- "log10pval"

  # Handle rescaling
  trans <- list(trans = "identity", breaks = ggplot2::waiver())
  if (isTRUE(rescale)) {
    get_transform_jump <- utils::getFromNamespace("get_transform_jump", "ggmanh")
    get_transform <- utils::getFromNamespace("get_transform", "ggmanh")
    jump <- get_transform_jump(pval_transform(mpdata$signif))
    if ((ceiling(max(mpdata$data[[mpdata$pval.colname]]) / 5) * 5) / jump > rescale_ratio_threshold) {
      trans <- get_transform(mpdata$data, jump, mpdata$pval.colname, jump.rel.pos = signif_rel_pos)
    }
  }

  ylimit <- c(0, ifelse(identical(trans$trans, "identity"), NA, max(trans$breaks)))

  # Handle x-axis for single vs multiple chromosomes
  if (length(unique(mpdata$data[[chr_by]])) == 1) {
    pos <- mpdata$true.pos.colname
    x_break <- ggplot2::waiver()
    x_break_label <- ggplot2::waiver()
    x_limits <- NULL
    chrname <- as.character(unique(mpdata$data[[chr_by]]))
    xlab <- xlab %||% ifelse(
      startsWith(chrname, "chr") || startsWith(chrname, "Chr"),
      chrname,
      paste0("Chromosome ", chrname)
    )
  } else {
    calc_new_pos_ <- utils::getFromNamespace("calc_new_pos_", "ggmanh")
    if (mpdata$pos.colname == "new_pos_unscaled") {
      mpdata$data$new_pos_unscaled <- calc_new_pos_(
        mpdata$data$new_pos_unscaled,
        mpdata$data[[mpdata$chr.colname]],
        mpdata$chr.pos.info
      )
    }
    pos <- mpdata$pos.colname
    x_break <- mpdata$chr.pos.info$center_pos
    x_break_label <- mpdata$chr.labels
    x_limits <- c(min(mpdata$chr.pos.info$start_pos), max(mpdata$chr.pos.info$end_pos))
  }
  xlab <- xlab %||% "Chromosome"

  # Set up expansion
  expand <- c(0.02, 0.01, 0.025, 0.01)
  if (isTRUE(signif_label)) {
    expand[1] <- 0.1
  }
  expand <- normalize_expansion(expand, "continuous", "continuous")

  # Build the main plot
  p <- ggplot(
    mpdata$data,
    ggplot2::aes(x = !!rlang::sym(pos), y = !!rlang::sym(mpdata$pval.colname), color = !!rlang::sym(chr_by))
  ) +
    ggplot2::geom_point(size = pt_size, shape = pt_shape, alpha = pt_alpha) +
    ggplot2::scale_color_manual(values = mpdata$chr.col, guide = "none") +
    ggplot2::scale_y_continuous(
      trans = trans$trans,
      breaks = trans$breaks,
      expand = expand$y,
      limits = ylimit
    ) +
    ggplot2::scale_x_continuous(
      name = xlab,
      breaks = x_break,
      labels = x_break_label,
      expand = expand$x,
      limits = x_limits
    ) +
    ggplot2::geom_hline(
      yintercept = pval_transform(mpdata$signif),
      linetype = "dashed",
      color = mpdata$signif.col
    )

  # Apply theme
  p <- p + do.call(theme, theme_args)

  # Get base_size for dynamic text sizing
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")

  # Manhattan plots typically have many points on both axes (continuous)
  # Use slightly smaller text (10pt) for better readability with many labels
  axis_size <- 10 * base_size / 12

  p <- p + ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "none",
    axis.text.x = ggplot2::element_text(
      size = axis_size,
      colour = "black"
    ),
    axis.text.y = ggplot2::element_text(
      size = axis_size,
      colour = "black"
    )
  )

  # Add labels
  p <- p + ggplot2::labs(x = xlab, y = ylab, title = title, subtitle = subtitle)

  # Add significance labels
  if (isTRUE(signif_label)) {
    p <- p + ggplot2::geom_text(
      data.frame(
        x = if (signif_label_pos == "left") -Inf else Inf,
        y = pval_transform(mpdata$signif),
        label = mpdata$signif,
        color = signif_color %||% c("black", rep("grey80", length(mpdata$signif) - 1))
      ),
      mapping = ggplot2::aes(x = !!rlang::sym("x"), y = !!rlang::sym("y"), label = !!rlang::sym("label"), color = I(!!rlang::sym("color"))),
      hjust = if (signif_label_pos == "left") -0.5 else 1.5,
      vjust = -0.5,
      size = signif_label_size,
      inherit.aes = FALSE
    )
  }

  # Add labels
  if (!is.null(label_by)) {
    if (is.null(label_fg)) {
      p <- p + ggrepel::geom_label_repel(
        ggplot2::aes(label = !!rlang::sym(label_by), color = !!rlang::sym(chr_by)),
        size = label_size,
        min.segment.length = 0,
        max.overlaps = 100
      )
    } else {
      p <- p + ggrepel::geom_label_repel(
        ggplot2::aes(label = !!rlang::sym(label_by)),
        color = label_fg,
        size = label_size,
        min.segment.length = 0,
        max.overlaps = 100
      )
    }
  }

  # Add rescale annotation if needed
  if (isTRUE(rescale) && !identical(trans$trans, "identity")) {
    jump_tick_size <- 3.5

    p <- p +
      ggplot2::theme(
        axis.ticks.y = ggplot2::element_line(linetype = trans$y_axis_linetype)
      ) +
      ggplot2::annotate(
        geom = "point",
        shape = "=",
        x = -Inf,
        y = trans$jump,
        size = jump_tick_size
      ) +
      ggplot2::coord_cartesian(clip = "off")
  }

  # Add highlighting
  if (is.numeric(highlight)) {
    hidata <- mpdata$data[highlight, , drop = FALSE]
  } else if (is.character(highlight)) {
    hidata <- mpdata$data %>% dplyr::filter(!!rlang::parse_expr(highlight))
  } else {
    hidata <- NULL
  }

  if (!is.null(hidata) && nrow(hidata) > 0) {
    if (is.null(highlight_color)) {
      p <- p + ggplot2::geom_point(
        data = hidata,
        ggplot2::aes(x = !!rlang::sym(pos), y = !!rlang::sym(mpdata$pval.colname), color = !!rlang::sym(chr_by)),
        size = highlight_size,
        shape = highlight_shape,
        alpha = highlight_alpha
      )
    } else {
      p <- p + ggplot2::geom_point(
        data = hidata,
        ggplot2::aes(x = !!rlang::sym(pos), y = !!rlang::sym(mpdata$pval.colname)),
        size = highlight_size,
        shape = highlight_shape,
        color = highlight_color,
        alpha = highlight_alpha
      )
    }
  }

  # Set plot size attributes
  attr(p, "height") <- 4.5
  attr(p, "width") <- 0.4 * length(levels(mpdata$data[[chr_by]]))

  return(p)
}
