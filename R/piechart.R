#' Pie Chart (Atomic)
#'
#' @description
#' Creates a single pie chart without data splitting
#'
#' @inheritParams parameters
#' @param x Column name for the x-axis (categories). Will be converted to factor.
#' @param y Column name for the y-axis (values). If NULL, counts will be used.
#' @param label Column to use for labels. NULL means no label.
#'   Default is the same as y. If y is NULL, use ".y" to specify the count as the label.
#' @param clockwise Logical value to draw the pie chart clockwise or counter-clockwise.
#' @param keep_empty Keep empty factor levels
#'
#' @return A ggplot object
#' @keywords internal
#' @importFrom rlang sym syms := "%||%"
#' @importFrom dplyr lead if_else mutate group_by summarise n ungroup
#' @importFrom tidyr complete replace_na
#' @importFrom ggplot2 ggplot aes geom_col coord_polar scale_fill_manual labs
#' @importFrom ggplot2 element_blank guide_legend theme
#' @importFrom ggrepel geom_label_repel
PieChartAtomic <- function(
    data, x, y = NULL, label = y, clockwise = TRUE,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "free_y",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    keep_empty = FALSE,
    ...) {
  # Get ggplot function (with gglogger support)
  ggplot <- get_ggplot()

  # Get base size for text scaling
  base_size <- theme_args$base_size %||% 12
  text_size_scale <- base_size / 12

  # Validate and process columns
  x <- validate_columns(data, x, force_factor = TRUE)
  y <- validate_columns(data, y)

  # If y is NULL, compute counts for each category
  if (is.null(y)) {
    data <- data %>%
      dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
      dplyr::summarise(.y = dplyr::n(), .groups = "drop")
    y <- ".y"
  }

  # Validate label column
  label <- validate_columns(data, label)

  # Validate facet_by
  concated_facet_by <- validate_columns(
    data, facet_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE
  )

  # Fill missing factor levels with 0 if keep_empty is TRUE
  if (isTRUE(keep_empty)) {
    .handle_one_facet <- function(df) {
      df <- df %>% tidyr::complete(!!sym(x))
      df[[y]] <- df[[y]] %>% tidyr::replace_na(0)
      df
    }

    if (is.null(facet_by)) {
      data <- .handle_one_facet(data)
    } else {
      dsplit <- split(data, data[[concated_facet_by]])
      dsplit <- lapply(names(dsplit), function(n) {
        df <- .handle_one_facet(dsplit[[n]])
        df[[concated_facet_by]] <- n
        df
      })
      data <- do.call(rbind, dsplit)
      rm(dsplit)
    }
  }

  # Reverse factor levels for clockwise ordering
  if (isTRUE(clockwise)) {
    data[[x]] <- factor(data[[x]], levels = rev(levels(data[[x]])))
  }
  data <- data[order(data[[x]]), , drop = FALSE]

  # Calculate label positions for pie slices
  .pos_df_one_facet <- function(df) {
    df %>% dplyr::mutate(
      csum = rev(cumsum(rev(!!sym(y)))),
      pos = !!sym(y) / 2 + dplyr::lead(!!sym("csum"), 1),
      pos = dplyr::if_else(is.na(!!sym("pos")), !!sym(y) / 2, !!sym("pos"))
    )
  }

  if (is.null(facet_by)) {
    pos_df <- .pos_df_one_facet(data)
  } else {
    pos_df <- do.call(
      rbind,
      lapply(split(data, data[[concated_facet_by]]), .pos_df_one_facet)
    )
  }

  # Get color palette (reverse order for clockwise)
  colors <- get_palette(
    if (isTRUE(clockwise)) rev(levels(data[[x]])) else levels(data[[x]]),
    palette = palette,
    palcolor = palcolor
  )

  # Build pie chart using polar coordinates
  p <- ggplot(data, ggplot2::aes(x = "", y = !!sym(y), fill = !!sym(x))) +
    ggplot2::geom_col(width = 1, alpha = alpha, color = "white") +
    ggplot2::scale_fill_manual(
      name = x,
      drop = !keep_empty,
      values = colors,
      guide = ggplot2::guide_legend(reverse = clockwise)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
    ggplot2::coord_polar(theta = "y") +
    do.call(theme, theme_args) +
    ggplot2::theme(
      aspect.ratio = aspect.ratio,
      legend.position = legend.position,
      legend.direction = legend.direction,
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    )

  # Add slice labels if requested
  if (!is.null(label)) {
    p <- p + ggrepel::geom_label_repel(
      data = pos_df,
      ggplot2::aes(y = !!sym("pos"), label = !!sym(label)),
      nudge_x = 0.1,
      color = "grey20",
      fill = "#fcfcfc",
      size = text_size_scale * 3
    )
  }

  # Calculate plot dimensions based on legend position
  height <- 4.5
  width <- 4.5
  if (!identical(legend.position, "none")) {
    if (legend.position %in% c("right", "left")) {
      width <- width + 1
    } else if (legend.direction == "horizontal") {
      height <- height + 1
    } else {
      width <- width + 2
    }
  }

  # Store dimensions as plot attributes
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


#' Pie Chart
#'
#' @description
#' Creates pie charts to illustrate numerical proportion of each group.
#' Supports splitting by groups, faceting, and custom color palettes.
#'
#' @param data A data frame
#' @param x Column name for categories. Will be converted to factor.
#' @param y Column name for values. If NULL, counts will be used.
#' @param label Column to use for labels. Default is same as y.
#'   If y is NULL, use ".y" to specify counts as labels.
#' @param split_by Column(s) to split the data by, creating separate plots.
#'   Multiple columns will be concatenated with split_by_sep.
#' @param split_by_sep Separator for concatenating multiple split_by columns.
#' @param clockwise Whether to draw pie chart clockwise (default: TRUE)
#' @param facet_by Column(s) to facet by (max 2 columns).
#' @param facet_scales Scale type for facets: "fixed", "free", "free_x", or "free_y"
#' @param facet_ncol Number of columns for faceting.
#' @param facet_nrow Number of rows for faceting.
#' @param facet_byrow Whether to fill facets by row.
#' @param theme Theme name or function. Default is "theme_ggforge".
#' @param theme_args List of arguments to pass to the theme function.
#' @param palette Color palette name. Default is "Paired".
#' @param palcolor Custom colors (overrides palette).
#' @param alpha Transparency level (0-1). Default is 1.
#' @param aspect.ratio Aspect ratio of the plot. Default is 1.
#' @param legend.position Legend position ("none", "left", "right", "bottom", "top").
#' @param legend.direction Legend direction ("horizontal" or "vertical").
#' @param title Plot title. Can be a string or function.
#' @param subtitle Plot subtitle.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param keep_empty Keep empty factor levels.
#' @param combine Whether to combine plots when split_by is used.
#' @param nrow Number of rows when combining plots.
#' @param ncol Number of columns when combining plots.
#' @param byrow Whether to arrange plots by row when combining.
#' @param seed Random seed for reproducibility. Default is 8525.
#' @param axes Axis handling when combining plots.
#' @param axis_titles Axis title handling when combining plots.
#' @param guides Guide handling when combining plots.
#' @param design Custom design for combining plots.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot object, patchwork object (if combine=TRUE), or list of plots
#' @export
#'
#' @examples
#' # Create sample data
#' data <- data.frame(
#'   x = c("A", "B", "C", "D", "E", "F", "G", "H"),
#'   y = c(10, 8, 16, 4, 6, 12, 14, 2),
#'   group = c("G1", "G1", "G2", "G2", "G3", "G3", "G4", "G4"),
#'   facet = c("F1", "F2", "F3", "F4", "F1", "F2", "F3", "F4")
#' )
#'
#' # Basic pie chart
#' PieChart(data, x = "x", y = "y")
#'
#' # Counter-clockwise
#' PieChart(data, x = "x", y = "y", clockwise = FALSE)
#'
#' # With labels
#' PieChart(data, x = "x", y = "y", label = "group")
#'
#' # With faceting
#' PieChart(data, x = "x", y = "y", facet_by = "facet")
#'
#' # Split by group
#' PieChart(data, x = "x", y = "y", split_by = "group")
#'
#' # Custom palettes per split
#' PieChart(data,
#'   x = "x", y = "y", split_by = "group",
#'   palette = list(G1 = "Reds", G2 = "Blues", G3 = "Greens", G4 = "Purples")
#' )
#'
#' # Use counts (y from count)
#' PieChart(data, x = "group")
#'
#' # With count labels
#' PieChart(data, x = "group", label = ".y")
PieChart <- function(
    data, x, y = NULL, label = y,
    split_by = NULL, split_by_sep = "_",
    clockwise = TRUE,
    facet_by = NULL, facet_scales = "free_y",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Paired", palcolor = NULL,
    alpha = 1, aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL,
    xlab = NULL, ylab = NULL,
    keep_empty = FALSE,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
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

  # Validate columns
  x <- validate_columns(data, x, force_factor = TRUE)
  y <- validate_columns(data, y)
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
    atomic_fn = PieChartAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}
