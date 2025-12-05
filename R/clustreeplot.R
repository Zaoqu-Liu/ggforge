#' Clustree Plot
#'
#' @description
#' Visualizes clusterings at different resolutions using a tree structure.
#' This function wraps the clustree package with consistent ggforge styling.
#'
#' @inheritParams parameters
#' @param prefix A character string of the prefix of the columns to plot.
#'   The columns with the prefix will be used to plot the tree.
#' @param flip A logical value to flip the tree (horizontal layout).
#' @param edge_palette A character string of the palette name to color the edges.
#' @param edge_palcolor A character vector of colors to color the edges.
#' @param expand Expansion values for plot limits (CSS-like notation).
#' @param ... Other arguments passed to \code{clustree::clustree}.
#'
#' @return A ggplot object or combined plots (if split_by is used)
#' @export
#' @importFrom stats complete.cases
#' @importFrom dplyr %>% select starts_with
#' @importFrom ggplot2 scale_color_manual coord_cartesian coord_flip element_line
#' @importFrom ggplot2 element_blank element_text geom_text labs scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous scale_y_reverse
#' @importFrom rlang sym "%||%"
#'
#' @examples
#' \donttest{
#' set.seed(8525)
#' N <- 100
#' data <- data.frame(
#'   p.0.4 = sample(LETTERS[1:5], N, replace = TRUE),
#'   p.0.5 = sample(LETTERS[1:6], N, replace = TRUE),
#'   p.0.6 = sample(LETTERS[1:7], N, replace = TRUE),
#'   p.0.7 = sample(LETTERS[1:8], N, replace = TRUE),
#'   p.0.8 = sample(LETTERS[1:9], N, replace = TRUE),
#'   p.0.9 = sample(LETTERS[1:10], N, replace = TRUE),
#'   p.1 = sample(LETTERS[1:30], N, replace = TRUE),
#'   split = sample(1:2, N, replace = TRUE)
#' )
#'
#' ClustreePlot(data, prefix = "p")
#' ClustreePlot(data, prefix = "p", flip = TRUE)
#' ClustreePlot(data, prefix = "p", split_by = "split")
#' ClustreePlot(data,
#'   prefix = "p", split_by = "split",
#'   palette = c("1" = "Set1", "2" = "Paired")
#' )
#' }
ClustreePlot <- function(
    data,
    prefix,
    flip = FALSE,
    split_by = NULL,
    split_by_sep = "_",
    palette = "Paired",
    palcolor = NULL,
    edge_palette = "Spectral",
    edge_palcolor = NULL,
    alpha = 0.85,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    expand = c(0.1, 0.1),
    theme = "theme_ggforge",
    theme_args = list(),
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

  # Validate split_by columns
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
    atomic_fn = ClustreePlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = NULL,
    combine = combine
  )
}

#' Clustree Plot Atomic
#'
#' @description
#' Creates a single clustree plot without splitting. This is the internal
#' function that generates the actual visualization.
#'
#' @inheritParams parameters
#' @keywords internal
#' @importFrom stats complete.cases
#' @importFrom dplyr %>% select starts_with
#' @importFrom ggplot2 scale_color_manual coord_cartesian coord_flip element_line
#' @importFrom ggplot2 element_blank element_text geom_text labs scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous scale_y_reverse
#' @importFrom rlang sym "%||%"
ClustreePlotAtomic <- function(
    data,
    prefix,
    flip = FALSE,
    alpha = 0.85,
    palette = "Paired",
    palcolor = NULL,
    edge_palette = "Spectral",
    edge_palcolor = NULL,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    expand = c(0.1, 0.1),
    theme = "theme_ggforge",
    theme_args = list(),
    ...) {
  # Check for required packages
  if (!requireNamespace("clustree", quietly = TRUE)) {
    stop(
      "Package 'clustree' is required for ClustreePlot. ",
      "Please install it with: install.packages('clustree')",
      call. = FALSE
    )
  }

  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop(
      "Package 'ggraph' is required for ClustreePlot. ",
      "Please install it with: install.packages('ggraph')",
      call. = FALSE
    )
  }

  # Filter data to columns matching the prefix
  data <- data %>% dplyr::select(dplyr::starts_with(prefix))
  data <- data[stats::complete.cases(data), , drop = FALSE]

  # Validate data
  if (ncol(data) == 0 || nrow(data) == 0) {
    stop(
      "No data found with prefix '", prefix, "'. ",
      "Check that column names match the prefix pattern.",
      call. = FALSE
    )
  }

  # Extract resolution values from column names
  resolutions <- substring(colnames(data), nchar(prefix) + 1)
  nres <- length(resolutions)

  # Detect and handle different separator styles (_. or .)
  if (all(startsWith(resolutions, "_"))) {
    prefix <- paste0(prefix, "_")
    resolutions <- substring(resolutions, 2)
  } else if (all(startsWith(resolutions, "."))) {
    prefix <- paste0(prefix, ".")
    resolutions <- substring(resolutions, 2)
  }

  # Convert resolutions to numeric and standardize column names
  resolutions <- as.numeric(resolutions)
  colnames(data) <- paste0(prefix, resolutions)
  resolutions <- sort(resolutions)
  nres <- length(resolutions)

  # Calculate number of clusters at maximum resolution for sizing
  max_res_col <- paste0(prefix, max(resolutions))
  max_clusters <- length(unique(data[[max_res_col]]))

  # Prepare arguments for clustree with sensible defaults
  clustree_args <- list(...)
  clustree_args$x <- data
  clustree_args$prefix <- prefix
  clustree_args$node_alpha <- clustree_args$node_alpha %||% alpha
  clustree_args$edge_width <- clustree_args$edge_width %||% 0.9
  clustree_args$show_axis <- clustree_args$show_axis %||% TRUE
  clustree_args$layout <- clustree_args$layout %||% "sugiyama"
  clustree_args$node_size_range <- clustree_args$node_size_range %||% c(6, 12)
  clustree_args$node_text_size <- clustree_args$node_text_size %||% 3
  clustree_args$node_text_colour <- clustree_args$node_text_colour %||%
    clustree_args$node_text_color %||% "black"

  # Register clustree with gglogger if enabled
  clustree_fn <- if (getOption("ggforge.gglogger.enabled", FALSE)) {
    if (requireNamespace("gglogger", quietly = TRUE)) {
      gglogger::register(clustree::clustree)
    } else {
      clustree::clustree
    }
  } else {
    clustree::clustree
  }

  # Create the base clustree plot
  p <- suppressMessages(do.call(clustree_fn, clustree_args))

  # Normalize expansion values (CSS-like notation support)
  expand_vals <- normalize_expansion(
    expand,
    x_type = "continuous",
    y_type = "continuous"
  )

  # Get base_size for consistent text sizing
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  axis_text_size <- 10 * base_size / 12

  # Customize plot with ggforge styling
  p <- suppressMessages({
    p +
      ggplot2::geom_text(
        ggplot2::aes(label = !!sym("cluster"), x = !!sym("x"), y = !!sym("y")),
        size = clustree_args$node_text_size,
        color = clustree_args$node_text_colour
      ) +
      ggplot2::scale_color_manual(
        values = get_palette(
          x = 1:nres,
          n = nres,
          palette = palette,
          palcolor = palcolor,
          keep_names = FALSE
        ),
        guide = "none"
      ) +
      ggraph::scale_edge_color_gradientn(
        name = "count",
        n.breaks = 5,
        colors = get_palette(
          x = 1:100,
          n = 100,
          palette = edge_palette,
          palcolor = edge_palcolor,
          keep_names = FALSE,
          type = "continuous"
        ),
        na.value = "grey80",
        guide = "none"
      ) +
      do.call(theme, theme_args) +
      ggplot2::theme(
        aspect.ratio = aspect.ratio,
        axis.title.x = ggplot2::element_text(),
        axis.title.y = ggplot2::element_text(),
        axis.text.x = ggplot2::element_text(size = axis_text_size, colour = "black"),
        axis.text.y = ggplot2::element_text(size = axis_text_size, colour = "black"),
        legend.position = legend.position,
        legend.direction = legend.direction
      ) +
      ggplot2::guides(edge_color = "none") +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        x = xlab %||% "",
        y = ylab %||% sub("\\.|_$", "", prefix)
      ) +
      ggplot2::scale_x_continuous(expand = expand_vals$x)
  })

  # Calculate dynamic plot dimensions based on tree size
  height <- nres * max_clusters / 20
  width <- if (max_clusters > 20) {
    11
  } else if (max_clusters > 15) {
    9
  } else {
    7
  }

  # Apply coordinate transformation based on flip parameter
  if (isTRUE(flip)) {
    # Flip to horizontal layout
    attr(p, "height") <- width
    attr(p, "width") <- height
    p <- suppressMessages({
      p +
        ggplot2::scale_y_reverse(
          breaks = nres:1,
          labels = resolutions,
          expand = expand_vals$y
        ) +
        ggplot2::coord_flip(clip = "off") +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_line(colour = "grey80", linetype = 2),
          panel.grid.major.y = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    })
  } else {
    # Keep vertical layout
    attr(p, "height") <- height
    attr(p, "width") <- width
    p <- suppressMessages({
      p +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::scale_y_continuous(
          breaks = nres:1,
          labels = resolutions,
          expand = expand_vals$y
        ) +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(colour = "grey80", linetype = 2),
          axis.line.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    })
  }

  return(p)
}
