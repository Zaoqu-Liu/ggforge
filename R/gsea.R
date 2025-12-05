#' Get the running enrichment score of a gene set
#'
#' @description
#' Calculates the GSEA running enrichment score using the weighted Kolmogorov-Smirnov-like statistic
#'
#' @param genes A vector of genes in the gene set
#' @param gene_ranks A numeric vector of gene ranks with gene names
#' @param exponent A numeric value to raise the gene ranks to (default: 1)
#' @param hits_only A logical value to return only the running enrichment score of the hits (default: TRUE)
#' @return A numeric vector of the running enrichment score
#' @keywords internal
gsea_running_score <- function(genes, gene_ranks, exponent = 1, hits_only = TRUE) {
  # Keep only genes present in the ranking
  genes <- intersect(genes, names(gene_ranks))
  N <- length(gene_ranks)
  Nh <- length(genes)

  # Calculate hit and miss probabilities
  Phit <- Pmiss <- numeric(N)
  hits <- names(gene_ranks) %in% genes
  Phit[hits] <- abs(gene_ranks[hits])^exponent

  # Normalize hit probability (avoid division by zero)
  NR <- sum(Phit)
  if (NR == 0) NR <- 1e-3
  Phit <- cumsum(Phit) / NR

  # Calculate miss probability
  Pmiss[!hits] <- 1 / (N - Nh)
  Pmiss <- cumsum(Pmiss)

  # Running enrichment score
  runningScore <- Phit - Pmiss

  if (hits_only) {
    runningScore <- runningScore[hits]
    names(runningScore) <- names(gene_ranks)[hits]
    runningScore[genes]
  } else {
    names(runningScore) <- names(gene_ranks)
    runningScore
  }
}

#' Prepare fgsea result for plotting
#'
#' @description
#' Converts fgsea package output format to DOSE-compatible format for plotting
#'
#' @param data A data frame of fgsea results
#' @return A data frame with standardized column names for plotting
#' @keywords internal
prepare_fgsea_result <- function(data) {
  # Rename pathway to ID and Description
  data$ID <- data$pathway
  data$Description <- data$pathway
  data$pathway <- NULL

  # Rename p-value columns
  data$pvalue <- data$pval
  data$pval <- NULL
  data$p.adjust <- data$padj
  data$p.adjust[is.na(data$p.adjust)] <- 1
  data$padj <- NULL

  # Convert leadingEdge to core_enrichment format
  if (is.character(data$leadingEdge)) {
    data$leadingEdge <- strsplit(data$leadingEdge, ",")
  }
  data$core_enrichment <- sapply(data$leadingEdge, paste0, collapse = "/")
  data$leadingEdge <- NULL

  # Handle NA in NES
  data$NES[is.na(data$NES)] <- 0

  data
}

#' GSEA Summary Plot
#'
#' @description
#' Creates a summary visualization of GSEA (Gene Set Enrichment Analysis) results,
#' showing normalized enrichment scores (NES) with integrated line plots for each term.
#'
#' @inheritParams parameters
#' @param data A data frame of GSEA results
#'  For example, from `DOSE::gseDO()` or `fgsea::fgsea()`.
#'  Required columns are `ID`, `Description`, `NES`, `p.adjust`, `pvalue`.
#'  The `ID` column is used to match the gene sets.
#' @param in_form The format of the input data
#'  * `fgsea`: The input data is from the `fgsea` package.
#'  * `dose`: The input data is from the `DOSE` package.
#'  * `auto`: Automatically detect the format of the input data.
#'  When "leadingEdge" is in the input data, it will be treated as "fgsea"; otherwise,
#'  if "core_enrichment" is in the input data, it will be treated as "dose".
#' @param gene_ranks A numeric vector of gene ranks with genes as names
#'  The gene ranks are used to plot the gene sets.
#'  If `gene_ranks` is a character vector starting with `@`, the gene ranks will be taken from the attribute of `data`.
#' @param gene_sets A list of gene sets, typically from a record of a GMT file
#'  The names of the list should match the `ID` column of `data`.
#'  If `gene_sets` is a character vector starting with `@`, the gene sets will be taken from the attribute of `data`.
#' @param top_term An integer to select the top terms
#' @param metric The metric to use for the significance of the terms
#' Typically the column name of p values or adjusted p values.
#' It is also used to select the top terms.
#' @param cutoff The cutoff for the significance of the terms
#'  The terms will not be filtered with this cutoff; they are only filtered by the `top_term` ranked by the `metric`.
#'  The cutoff here is used to show the significance of the terms on the plot.
#'  For the terms that are not significant, the color will be grey.
#' @param character_width The width of the characters in the y-axis
#' @param line_plot_size The size of the line plots
#' @param metric_name The name of the metric to show in the color bar
#' @param nonsig_name The name of the legend for the nonsignificant terms
#' @param linewidth The width of the lines in the line plots
#' @param line_by The method to calculate the line plots.
#'  * `prerank`: Use the gene ranks as heights to plot the line plots.
#'  * `running_score`: Use the running score to plot the line plots.
#'
#' @return A ggplot object with height and width attributes
#' @export
#' @importFrom scales pretty_breaks scientific
#' @importFrom ggplot2 geom_linerange layer_scales theme_void ylim annotation_custom
#' @importFrom ggplot2 geom_vline scale_color_gradientn guide_colorbar scale_y_continuous
#' @importFrom ggplot2 scale_x_continuous labs theme element_line geom_rect geom_point ggplotGrob
#' @importFrom dplyr slice_min
#' @importFrom rlang sym %||%
#' @importFrom stringr str_wrap
#' @examples
#' \donttest{
#' data(gsea_example)
#'
#' # Basic usage
#' GSEASummaryPlot(gsea_example)
#'
#' # Use running score instead of prerank
#' GSEASummaryPlot(gsea_example, line_by = "running_score")
#'
#' # Adjust significance cutoff
#' GSEASummaryPlot(gsea_example, cutoff = 0.01)
#'
#' # Show more terms
#' GSEASummaryPlot(gsea_example, top_term = 15)
#'
#' # Customize color palette
#' GSEASummaryPlot(gsea_example, palette = "RdYlBu")
#'
#' # Adjust character width for long pathway names
#' GSEASummaryPlot(gsea_example, character_width = 70)
#' }
GSEASummaryPlot <- function(
    data, in_form = c("auto", "dose", "fgsea"), gene_ranks = "@gene_ranks", gene_sets = "@gene_sets",
    top_term = 10, metric = "p.adjust", cutoff = 0.05, character_width = 50, line_plot_size = 0.25,
    metric_name = metric, nonsig_name = "Insignificant", linewidth = 0.2,
    line_by = c("prerank", "running_score"), title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    alpha = 0.6, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    theme = "theme_ggforge", theme_args = list(), palette = "Spectral", palcolor = NULL,
    seed = 8525, ...) {
  # Validate common arguments
  validate_common_args(
    seed = seed,
    theme = theme,
    palette = palette,
    alpha = alpha,
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Set seed
  set.seed(seed)

  # Match arguments
  in_form <- match.arg(in_form)
  line_by <- match.arg(line_by)

  # Process theme
  theme <- process_theme(theme)

  # Get ggplot function (support gglogger)
  ggplot <- get_ggplot()

  # Convert gseaResult to data frame
  if (inherits(data, "gseaResult")) {
    data <- as.data.frame(data)
  }

  # Extract gene_ranks from attribute if needed
  if (is.character(gene_ranks) && length(gene_ranks) == 1 && startsWith(gene_ranks, "@")) {
    gene_ranks <- attr(data, substring(gene_ranks, 2))
  }
  if (is.null(gene_ranks)) {
    stop("'gene_ranks' must be provided", call. = FALSE)
  }
  if (is.null(names(gene_ranks))) {
    stop("'gene_ranks' must have names", call. = FALSE)
  }
  if (!is.numeric(gene_ranks)) {
    stop("'gene_ranks' must be numeric", call. = FALSE)
  }
  gene_ranks <- gene_ranks[order(-gene_ranks)]

  # Extract gene_sets from attribute if needed
  if (is.character(gene_sets) && length(gene_sets) == 1 && startsWith(gene_sets, "@")) {
    gene_sets <- attr(data, substring(gene_sets, 2))
  }
  if (is.null(gene_sets)) {
    stop("'gene_sets' must be provided", call. = FALSE)
  }
  if (!is.list(gene_sets)) {
    stop("'gene_sets' must be a list", call. = FALSE)
  }

  # Auto-detect input format
  if (in_form == "auto") {
    if ("leadingEdge" %in% colnames(data)) {
      in_form <- "fgsea"
    } else if ("core_enrichment" %in% colnames(data)) {
      in_form <- "dose"
    } else {
      stop("Cannot detect the input format. Please set 'in_form' to 'fgsea' or 'dose'.", call. = FALSE)
    }
  }

  # Convert fgsea format if needed
  if (in_form == "fgsea") {
    data <- prepare_fgsea_result(data)
  }

  # Select top terms
  if (!is.null(top_term)) {
    data <- dplyr::slice_min(data, !!sym(metric), n = top_term, with_ties = FALSE)
  }
  data$ID <- factor(data$ID, levels = rev(unique(data$ID)))
  data <- data[order(data$ID), , drop = FALSE]

  # Mark significant terms
  if (!is.null(cutoff)) {
    data$.signif <- data[[metric]] < cutoff
  } else {
    data$.signif <- TRUE
  }
  data$metric <- -log10(data[[metric]])

  # Validate and process Description column
  validate_columns(data, "Description", force_factor = TRUE)
  data$Description <- droplevels(data$Description)
  data$Description <- stringr::str_wrap(data$Description, width = character_width)
  data$Description <- factor(data$Description, levels = unique(data$Description))
  data$y <- as.integer(data$Description)
  sig_metrics <- data$metric[data$.signif]

  # Build base plot
  if (all(data$.signif)) {
    p <- ggplot(data, aes(x = !!sym("NES"), y = !!sym("y")))
  } else {
    p <- ggplot(data, aes(x = !!sym("NES"), y = !!sym("y"), fill = "")) +
      ggplot2::guides(fill = ggplot2::guide_legend(
        title = nonsig_name %||% "Insignificant",
        override.aes = list(color = "grey80", shape = 15, size = 4),
        order = 2
      ))
  }

  # Add dummy layer to get plot scales
  p <- p + ggplot2::geom_point(ggplot2::aes(color = !!sym("metric")), size = 0)
  x_range <- diff(ggplot2::layer_scales(p)$x$range$range)
  y_range <- diff(ggplot2::layer_scales(p)$y$range$range)

  # Get color palette
  colors <- get_palette(
    x = sig_metrics,
    palette = palette,
    palcolor = palcolor,
    alpha = alpha,
    type = "continuous"
  )

  # Create line plots for each term
  line_plot_list <- list()

  for (i in seq_len(nrow(data))) {
    if (isTRUE(data$.signif[i])) {
      # Calculate color based on metric value
      color <- colors[ceiling(data$metric[i] * length(colors) / max(sig_metrics))]
    } else {
      color <- "grey80"
    }

    hits <- intersect(gene_sets[[as.character(data$ID[i])]], names(gene_ranks))
    if (line_by == "running_score") {
      scores <- gsea_running_score(hits, gene_ranks)
    } else {
      scores <- gene_ranks[match(hits, names(gene_ranks))]
    }

    df <- data.frame(
      x = factor(hits, levels = names(gene_ranks)),
      y = 0,
      ymin = pmin(scores, 0),
      ymax = pmax(scores, 0)
    )
    yr <- max(max(df$ymax), abs(min(df$ymin))) * 1.05
    x_min <- -0.05 * length(gene_ranks)
    x_max <- 1.05 * length(gene_ranks)

    lp <- ggplot(df, ggplot2::aes(x = !!sym("x"), y = !!sym("y"))) +
      ggplot2::geom_rect(
        xmin = x_min, xmax = x_max, ymin = -yr, ymax = yr,
        fill = color
      ) +
      ggplot2::geom_linerange(ggplot2::aes(ymin = !!sym("ymin"), ymax = !!sym("ymax")), linewidth = linewidth) +
      ggplot2::ylim(-yr, yr) +
      ggplot2::theme_void() +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "none"
      )

    line_plot_list[[i]] <- ggplot2::annotation_custom(
      ggplot2::ggplotGrob(lp),
      xmin = data$NES[i] - x_range * line_plot_size / 2,
      xmax = data$NES[i] + x_range * line_plot_size / 2,
      ymin = i - y_range * line_plot_size / 8,
      ymax = i + y_range * line_plot_size / 8
    )
  }

  # Add vertical line at zero
  if (any(data$NES > 0) && any(data$NES < 0)) {
    p <- p + ggplot2::geom_vline(xintercept = 0, linetype = 2, color = "grey80")
  }

  # Add line plots
  p <- p + line_plot_list

  # Add color scale
  if (length(colors) == 0) {
    # In case all terms are not significant
    p <- p + ggplot2::scale_color_gradientn(colors = "grey80", guide = "none")
  } else {
    p <- p + ggplot2::scale_color_gradientn(
      name = metric_name,
      colors = colors,
      breaks = scales::pretty_breaks(n = 4),
      labels = function(x) scales::scientific(10^(-x), digits = 2),
      guide = ggplot2::guide_colorbar(
        frame.colour = "black", ticks.colour = "black",
        frame.linewidth = 0.3, ticks.linewidth = 0.3, title.hjust = 0, order = 1
      )
    )
  }

  # Finalize plot
  p <- p +
    ggplot2::scale_y_continuous(breaks = seq_len(nrow(data)), labels = data$Description) +
    ggplot2::scale_x_continuous(expand = c(0.05, x_range * line_plot_size / 2)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab %||% "NES", y = ylab %||% "")

  # Apply theme and automatic styling
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(
      aspect.ratio = aspect.ratio,
      panel.grid.major.y = ggplot2::element_line(colour = "grey80", linetype = 2)
    )

  # Apply data-driven styling
  # x is NES (continuous) -> size=10, y is Description (discrete) -> size=12
  text_size_scale <- base_size / 12
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = NULL,
    y_var = "Description",
    flip = FALSE,
    base_size = base_size,
    axis.text.x = ggplot2::element_text(
      size = 10 * text_size_scale,
      colour = "black"
    ),
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Calculate and set dimensions
  max_nchar_y <- min(max(nchar(levels(data$Description))), character_width)
  height <- nrow(data) * 0.65
  width <- max_nchar_y * 0.1 + 5
  if (!identical(legend.position, "none")) {
    if (legend.position %in% c("right", "left")) {
      width <- width + 1.5
    } else if (legend.direction == "horizontal") {
      height <- height + 2
    } else {
      height <- height + 3.5
    }
  }
  attr(p, "height") <- height
  attr(p, "width") <- width

  p
}

#' GSEA Plot Atomic
#'
#' @description
#' Creates a detailed GSEA plot for a single gene set, showing the running enrichment score,
#' gene positions, and ranked list metric.
#'
#' @inheritParams parameters
#' @inheritParams GSEASummaryPlot
#' @param gs The name of the gene set
#' @param genes The genes in the gene set
#' @param metric The metric to show in the subtitle
#' @param sample_coregenes A logical value to sample the core genes from the core_enrichment; if `FALSE`, the first `n_coregenes` will be used
#' @param line_width The width of the line in the running score plot
#' @param line_alpha The alpha of the line in the running score plot
#' @param line_color The color of the line in the running score plot
#' @param n_coregenes The number of core genes to label
#' @param genes_label The genes to label. If set, `n_coregenes` will be ignored
#' @param label_fg The color of the label text
#' @param label_bg The background color of the label
#' @param label_bg_r The radius of the background color of the label
#' @param label_size The size of the label text
#' @param ylab The label of the y-axis, will be shown on the right side
#' @keywords internal
#' @importFrom scales alpha
#' @importFrom ggplot2 ggtitle theme_classic annotate geom_hline geom_line geom_segment
#' @importFrom ggplot2 geom_point ylab xlab theme element_text element_rect geom_linerange
#' @importFrom ggplot2 element_blank element_line margin scale_x_continuous scale_y_continuous geom_vline
#' @importFrom ggrepel geom_text_repel
#' @importFrom patchwork plot_layout wrap_plots plot_spacer
#' @importFrom grid textGrob
#' @importFrom dplyr case_when
#' @importFrom grDevices colorRampPalette
GSEAPlotAtomic <- function(
    data, gene_ranks = "@gene_ranks", gs, genes, metric = "p.adjust", sample_coregenes = FALSE,
    line_width = 1.5, line_alpha = 1, line_color = "#6BB82D", n_coregenes = 10, genes_label = NULL,
    label_fg = "black", label_bg = "white", label_bg_r = 0.1, label_size = 4,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, ...) {
  # Get ggplot function (support gglogger)
  ggplot <- get_ggplot()

  # Filter data to single gene set
  if (nrow(data) > 1) {
    data <- data[data$ID == gs, , drop = FALSE]
  }
  if (nrow(data) == 0) {
    stop("Gene set ", gs, " is not in the data", call. = FALSE)
  }

  # Extract gene_ranks from attribute if needed
  if (is.character(gene_ranks) && length(gene_ranks) == 1 && startsWith(gene_ranks, "@")) {
    gene_ranks <- attr(data, substring(gene_ranks, 2))
  }
  if (is.null(gene_ranks)) {
    stop("'gene_ranks' must be provided", call. = FALSE)
  }
  if (is.null(names(gene_ranks))) {
    stop("'gene_ranks' must have names", call. = FALSE)
  }
  if (!is.numeric(gene_ranks)) {
    stop("'gene_ranks' must be numeric", call. = FALSE)
  }
  gene_ranks <- gene_ranks[order(-gene_ranks)]

  # Build title and subtitle
  title <- title %||% data$Description
  sig <- dplyr::case_when(
    data[[metric]] > 0.05 ~ "ns",
    data[[metric]] <= 0.05 & data[[metric]] > 0.01 ~ "*",
    data[[metric]] <= 0.01 & data[[metric]] > 0.001 ~ "**",
    data[[metric]] <= 0.001 & data[[metric]] > 0.0001 ~ "***",
    data[[metric]] <= 0.0001 ~ "****"
  )
  subtitle <- subtitle %||% paste0(
    "(NES=", round(data$NES, 3), ", ",
    metric, "=", format(data[[metric]], digits = 3, scientific = TRUE), ", ",
    sig, ")"
  )

  # Prepare data frame for plotting
  df <- data.frame(
    x = 1:length(gene_ranks),
    genes = names(gene_ranks),
    runningScore = gsea_running_score(genes, gene_ranks, hits_only = FALSE),
    ranks = gene_ranks,
    position = as.integer(names(gene_ranks) %in% genes)
  )

  index_max <- which.max(abs(df$runningScore))

  # Base theme for all panels
  p <- ggplot(df, ggplot2::aes(x = !!sym("x"))) +
    ggplot2::xlab(NULL) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "grey90", linetype = 2),
      panel.grid.minor = ggplot2::element_line(colour = "grey90", linetype = 2)
    ) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0))

  ############# The Running Score Panel #############
  p1 <- p +
    ggplot2::geom_hline(yintercept = 0, linetype = 1, color = "grey40") +
    # Running score line
    ggplot2::geom_line(ggplot2::aes(y = !!sym("runningScore")), color = line_color, linewidth = line_width, alpha = line_alpha) +
    ggplot2::annotate(
      geom = "segment", x = 0, xend = df$x[index_max],
      y = df$runningScore[index_max], yend = df$runningScore[index_max], linetype = 2
    ) +
    ggplot2::annotate(
      geom = "segment", x = df$x[index_max], xend = df$x[index_max],
      y = 0, yend = df$runningScore[index_max], linetype = 2
    ) +
    ggplot2::annotate(
      geom = "point", x = df$x[index_max], y = df$runningScore[index_max],
      fill = ifelse(data$NES < 0, "#5E34F5", "#F52323"), color = "black", size = 2.5,
      shape = ifelse(data$NES < 0, 25, 24)
    ) +
    ggplot2::ylab("Enrichment Score") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = "transparent", linewidth = 1),
      plot.margin = ggplot2::margin(t = 0.2, r = 0.2, b = 0, l = 0.2, unit = "cm"),
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
      axis.title.x = ggplot2::element_text(face = "bold", size = 12),
      axis.title.y = ggplot2::element_text(face = "bold", size = 12)
    ) +
    ggplot2::ggtitle(title, subtitle)

  # Label genes if requested
  if ((is.numeric(n_coregenes) && n_coregenes > 1) || length(genes_label) > 0) {
    if (length(genes_label) == 0) {
      genes_label_tmp <- unlist(strsplit(data$core_enrichment, "/"))
      n_coregenes <- min(n_coregenes, length(genes_label_tmp))
      if (isTRUE(sample_coregenes)) {
        genes_label_tmp <- sample(genes_label_tmp, n_coregenes, replace = FALSE)
      } else {
        genes_label_tmp <- df$genes[df$genes %in% genes_label_tmp][1:n_coregenes]
      }
    } else {
      genes_label_tmp <- genes_label
    }
    df_gene <- df[df$position == 1 & df$genes %in% genes_label_tmp, , drop = FALSE]
    gene_drop <- genes_label_tmp[!genes_label_tmp %in% df_gene$genes]
    if (length(gene_drop) > 0) {
      if (identical(data$ID, data$Description)) {
        warning("Gene ", paste(gene_drop, collapse = ","), " is not in the geneset ", data$ID, immediate. = TRUE)
      } else {
        warning("Gene ", paste(gene_drop, collapse = ","), " is not in the geneset ", data$ID, ": ", data$Description, immediate. = TRUE)
      }
    }
    x_nudge <- diff(range(df$x)) * 0.05
    y_nudge <- diff(range(df$runningScore)) * 0.05
    p1 <- p1 + ggplot2::geom_point(
      data = df_gene,
      mapping = ggplot2::aes(y = !!sym("runningScore")), color = "black"
    ) +
      ggrepel::geom_text_repel(
        data = df_gene,
        mapping = ggplot2::aes(y = !!sym("runningScore"), label = !!sym("genes")),
        min.segment.length = 0, max.overlaps = 100, segment.colour = "grey40",
        color = label_fg, bg.color = label_bg, bg.r = label_bg_r, size = label_size,
        nudge_x = ifelse(df_gene$runningScore >= 0, x_nudge, -x_nudge),
        nudge_y = ifelse(df_gene$runningScore > 0, -y_nudge, y_nudge)
      )
  }

  ############# The Line Plot Panel #############
  p2 <- ggplot(df, ggplot2::aes(x = !!sym("x"))) +
    ggplot2::geom_linerange(ggplot2::aes(ymax = !!sym("position")), ymin = 0, alpha = line_alpha) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(t = -0.1, b = 0, r = 0.2, l = 0.2, unit = "cm"),
      panel.border = ggplot2::element_rect(color = "black", fill = "transparent", linewidth = 1),
      axis.line.y = ggplot2::element_blank(), axis.line.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(), axis.text = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0))

  # Add colored background based on rank
  x <- df$x
  y <- y_raw <- df$ranks
  y[y > quantile(y_raw, 0.98)] <- quantile(y_raw, 0.98)
  y[y < quantile(y_raw, 0.02)] <- quantile(y_raw, 0.02)
  col <- rep("white", length(y))
  y_pos <- which(y > 0)
  if (length(y_pos) > 0) {
    y_pos_i <- cut(y[y_pos],
      breaks = seq(min(y[y_pos], na.rm = TRUE), max(y[y_pos], na.rm = TRUE), len = 100),
      include.lowest = TRUE
    )
    col[y_pos] <- grDevices::colorRampPalette(c("#F5DCDC", "#C40003"))(100)[y_pos_i]
  }

  y_neg <- which(y < 0)
  if (length(y_neg) > 0) {
    y_neg_i <- cut(y[y_neg],
      breaks = seq(min(y[y_neg], na.rm = TRUE), max(y[y_neg], na.rm = TRUE), len = 100),
      include.lowest = TRUE
    )
    col[y_neg] <- grDevices::colorRampPalette(c("#1D008F", "#DDDCF5"))(100)[y_neg_i]
  }
  xmin <- which(!duplicated(col))
  xmax <- xmin + as.numeric(table(col)[as.character(unique(col))])
  d <- data.frame(ymin = 0, ymax = 0.3, xmin = xmin, xmax = xmax, col = unique(col))
  p2 <- p2 +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = !!sym("xmin"), xmax = !!sym("xmax"), ymin = !!sym("ymin"), ymax = !!sym("ymax"), fill = I(!!sym("col"))),
      data = d,
      alpha = 0.95, inherit.aes = FALSE
    )

  ############# The Gene Ranking Panel #############
  p3 <- p +
    ggplot2::geom_segment(ggplot2::aes(x = !!sym("x"), xend = !!sym("x"), y = !!sym("ranks"), yend = 0), color = "grey30")

  cross_x <- median(df$x[which.min(abs(df$ranks))])
  if (max(df$ranks) > 0) {
    p3 <- p3 + ggplot2::annotate(geom = "text", x = 0, y = Inf, vjust = 1.4, hjust = 0, color = "#C81A1F", size = 4, label = " Positively correlated")
  }
  if (min(df$ranks) < 0) {
    p3 <- p3 + ggplot2::annotate(geom = "text", x = Inf, y = -Inf, vjust = -0.5, hjust = 1.02, color = "#3C298C", size = 4, label = "Negtively correlated ")
  }
  if (max(df$ranks) > 0 && min(df$ranks) < 0) {
    p3 <- p3 + ggplot2::geom_vline(xintercept = cross_x, linetype = 2, color = "black") +
      ggplot2::annotate(geom = "text", y = 0, x = cross_x, vjust = ifelse(diff(abs(range(df$ranks))) > 0, -0.3, 1.3), size = 4, label = paste0("Zero cross at ", cross_x))
  }
  p3 <- p3 + ggplot2::ylab("Ranked List Metric") + ggplot2::xlab(xlab %||% "Rank in Ordered Dataset") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = -0.1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
      axis.line = ggplot2::element_blank(), axis.line.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = "transparent", linewidth = 1),
      axis.title.x = ggplot2::element_text(face = "bold", size = 12),
      axis.title.y = ggplot2::element_text(face = "bold", size = 12)
    )

  # Combine panels
  if (!is.null(ylab)) {
    p4 <- grid::textGrob(label = ylab, rot = -90, hjust = 0.5)

    p <- patchwork::wrap_plots(p1, patchwork::plot_spacer(), p2, patchwork::plot_spacer(), p3, p4, heights = c(3.5, -0.19, 1, -0.24, 1.5), widths = c(12, .5)) +
      patchwork::plot_layout(axes = "collect", design = "AF\nBF\nCF\nDF\nEF")
    attr(p, "height") <- 6.5
    attr(p, "width") <- 8
  } else {
    p <- patchwork::wrap_plots(p1, patchwork::plot_spacer(), p2, patchwork::plot_spacer(), p3, ncol = 1, heights = c(3.5, -0.19, 1, -0.24, 1.5)) +
      patchwork::plot_layout(axes = "collect")
    attr(p, "height") <- 6.5
    attr(p, "width") <- 7.5
  }
  p
}

#' GSEA Plot
#'
#' @description
#' Creates detailed GSEA plots for one or more gene sets, showing the running enrichment score,
#' gene positions, and ranked list metric for each set.
#'
#' @inheritParams parameters
#' @inheritParams GSEASummaryPlot
#' @inheritParams GSEAPlotAtomic
#' @param gene_sets A list of gene sets, typically from a record of a GMT file
#' The names of the list should match the `ID` column of `data`.
#' If `gene_sets` is a character vector starting with `@`, the gene sets will be taken from the attribute of `data`.
#' The GSEA plots will be plotted for each gene set. So, the number of plots will be the number of gene sets.
#' If you only want to plot a subset of gene sets, you can subset the `gene_sets` before passing it to this function.
#' @param gs The names of the gene sets to plot
#' If `NULL`, all gene sets in `gene_sets` will be plotted.
#'
#' @return A ggplot object (if single plot) or combined plot object
#' @export
#' @examples
#' \donttest{
#' data(gsea_example)
#'
#' # Plot single gene set
#' GSEAPlot(gsea_example, gene_sets = attr(gsea_example, "gene_sets")[1])
#'
#' # Plot multiple gene sets
#' GSEAPlot(gsea_example, gene_sets = attr(gsea_example, "gene_sets")[1:4])
#'
#' # Label core genes
#' GSEAPlot(
#'   gsea_example,
#'   gene_sets = attr(gsea_example, "gene_sets")[1],
#'   n_coregenes = 5
#' )
#'
#' # Customize line appearance
#' GSEAPlot(
#'   gsea_example,
#'   gene_sets = attr(gsea_example, "gene_sets")[1],
#'   line_width = 2,
#'   line_color = "#FF6B6B"
#' )
#'
#' # Return separate plots instead of combined
#' plots <- GSEAPlot(
#'   gsea_example,
#'   gene_sets = attr(gsea_example, "gene_sets")[1:3],
#'   combine = FALSE
#' )
#' }
GSEAPlot <- function(
    data, in_form = c("auto", "dose", "fgsea"), gene_ranks = "@gene_ranks", gene_sets = "@gene_sets",
    gs = NULL, sample_coregenes = FALSE, line_width = 1.5, line_alpha = 1, line_color = "#6BB82D",
    n_coregenes = 10, genes_label = NULL, label_fg = "black", label_bg = "white",
    label_bg_r = 0.1, label_size = 4, title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, theme = "theme_ggforge", ...) {
  # Validate common arguments
  validate_common_args(seed = seed, theme = theme)

  # Set seed
  set.seed(seed)

  # Match arguments
  in_form <- match.arg(in_form)

  # Process theme
  theme <- process_theme(theme)

  # Convert gseaResult to data frame
  if (inherits(data, "gseaResult")) {
    data <- as.data.frame(data)
  }

  # Extract gene_ranks from attribute if needed
  if (is.character(gene_ranks) && length(gene_ranks) == 1 && startsWith(gene_ranks, "@")) {
    gene_ranks <- attr(data, substring(gene_ranks, 2))
  }
  if (is.null(gene_ranks)) {
    stop("'gene_ranks' must be provided", call. = FALSE)
  }
  if (is.null(names(gene_ranks))) {
    stop("'gene_ranks' must have names", call. = FALSE)
  }
  if (!is.numeric(gene_ranks)) {
    stop("'gene_ranks' must be numeric", call. = FALSE)
  }
  gene_ranks <- gene_ranks[order(-gene_ranks)]

  # Extract gene_sets from attribute if needed
  if (is.character(gene_sets) && length(gene_sets) == 1 && startsWith(gene_sets, "@")) {
    gene_sets <- attr(data, substring(gene_sets, 2))
  }
  if (is.null(gene_sets)) {
    stop("'gene_sets' must be provided", call. = FALSE)
  }
  if (!is.list(gene_sets)) {
    stop("'gene_sets' must be a list", call. = FALSE)
  }

  # Auto-detect input format
  if (in_form == "auto") {
    if ("leadingEdge" %in% colnames(data)) {
      in_form <- "fgsea"
    } else if ("core_enrichment" %in% colnames(data)) {
      in_form <- "dose"
    } else {
      stop("Cannot detect the input format. Please set 'in_form' to 'fgsea' or 'dose'.", call. = FALSE)
    }
  }

  # Convert fgsea format if needed
  if (in_form == "fgsea") {
    data <- prepare_fgsea_result(data)
  }

  # Filter to available gene sets
  gsnames <- intersect(as.character(data$ID), names(gene_sets))
  gene_sets <- gene_sets[gsnames]
  data <- data[as.character(data$ID) %in% gsnames, , drop = FALSE]
  gs <- gs %||% names(gene_sets)

  # Create plots for each gene set
  plots <- lapply(gs, function(g) {
    GSEAPlotAtomic(
      data,
      gene_ranks = gene_ranks, gs = g, genes = gene_sets[[g]],
      sample_coregenes = sample_coregenes, line_width = line_width, line_alpha = line_alpha,
      line_color = line_color, n_coregenes = n_coregenes, genes_label = genes_label,
      label_fg = label_fg, label_bg = label_bg, label_bg_r = label_bg_r, label_size = label_size,
      title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, ...
    )
  })
  names(plots) <- gs

  # Combine plots if requested
  combine_plots(plots,
    combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
    axes = axes, axis_titles = axis_titles, guides = guides, design = design
  )
}
