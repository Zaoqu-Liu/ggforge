#' Rarefaction Plot
#'
#' @description
#' Generate rarefaction/extrapolation curves for diversity analysis using iNEXT.
#' This function visualizes species diversity estimates with different levels of sampling effort.
#'
#' @inheritParams parameters
#' @param data An iNEXT object or a list of data that will be handled by [iNEXT::iNEXT].
#' @param type Three types of plots: sample-size-based rarefaction/extrapolation curve (\code{type = 1});
#'   sample completeness curve (\code{type = 2}); coverage-based rarefaction/extrapolation curve (\code{type = 3}).
#' @param se A logical variable to display confidence interval around the estimated sampling curve.
#'   Default to \code{NULL} which means TRUE if the data has the lower and upper bounds.
#' @param group_by A character string indicating how to group the data (color the lines).
#'   Possible values are "q" and "group"
#' @param group_by_sep A character string indicating how to separate the group_by column if both "q" and "group" are used.
#'   Default to "_".
#' @param group_name A character string indicating the name of the group, showing as the legend title.
#' @param pt_size A numeric value specifying the size of the points.
#' @param line_width A numeric value specifying the width of the lines.
#' @param split_by A character string indicating how to split the data and plots.
#'   Possible values are "q" and "group"
#' @param facet_by A character string indicating how to facet the data and plots.
#'   Possible values are "q" and "group"
#' @param ... Additional arguments to pass to [iNEXT::iNEXT] when \code{data} is not an iNEXT object.
#'
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @importFrom ggplot2 waiver fortify
#' @examples
#' \donttest{
#' set.seed(8525)
#' spider <- list(
#'   Girdled = c(46, 22, 17, 15, 15, 9, 8, 6, 6, 4, rep(2, 4), rep(1, 12)),
#'   Logged = c(
#'     88, 22, 16, 15, 13, 10, 8, 8, 7, 7, 7, 5, 4, 4, 4, 3, 3, 3, 3,
#'     2, 2, 2, 2, rep(1, 14)
#'   )
#' )
#'
#' RarefactionPlot(spider)
#' RarefactionPlot(spider, q = c(0, 1, 2), facet_by = "q")
#' RarefactionPlot(spider, q = c(0, 1, 2), split_by = "q")
#' RarefactionPlot(spider,
#'   q = c(0, 1, 2), split_by = "q",
#'   palette = c("0" = "Paired", "1" = "Set1", "2" = "Dark2")
#' )
#' RarefactionPlot(spider,
#'   q = c(0, 1, 2), group_by = "q",
#'   facet_by = "group", palette = "Set1", type = 3
#' )
#' }
RarefactionPlot <- function(
    data, type = 1, se = NULL,
    group_by = "group", group_by_sep = "_", group_name = NULL,
    split_by = NULL, split_by_sep = "_",
    theme = "theme_ggforge", theme_args = list(),
    palette = "Spectral", palcolor = NULL, alpha = 0.2,
    pt_size = 3, line_width = 1,
    facet_by = NULL, facet_scales = "fixed",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
    ...) {
  # Validate type parameter (must be 1, 2, or 3, or combination)
  stopifnot(
    "Invalid 'type' value. It must be 1, 2, or 3 or combination of them." =
      length(type) > 0 && length(setdiff(type, 1:3)) == 0
  )

  # Validate group_by, split_by, facet_by parameters
  # These can only be "q" (diversity order) or "group" (sample group)
  stopifnot(
    "Invalid 'group_by' value. It must be 'q' and/or 'group'." =
      is.null(group_by) || all(group_by %in% c("q", "group"))
  )
  stopifnot(
    "Invalid 'split_by' value. It must be 'q' and/or 'group'." =
      is.null(split_by) || all(split_by %in% c("q", "group"))
  )
  stopifnot(
    "Invalid 'facet_by' value. It must be 'q' and/or 'group'." =
      is.null(facet_by) || all(facet_by %in% c("q", "group"))
  )

  # Ensure no overlap between group_by, split_by, and facet_by
  stopifnot(
    "Invalid 'group_by'/'split_by'. They should not overlap." =
      is.null(group_by) || is.null(split_by) || length(intersect(group_by, split_by)) == 0
  )
  stopifnot(
    "Invalid 'group_by'/'facet_by'. They should not overlap." =
      is.null(group_by) || is.null(facet_by) || length(intersect(group_by, facet_by)) == 0
  )
  stopifnot(
    "Invalid 'split_by'/'facet_by'. They should not overlap." =
      is.null(split_by) || is.null(facet_by) || length(intersect(split_by, facet_by)) == 0
  )

  # Validate common arguments (but pass NULL for facet_by since we validate it separately)
  validate_common_args(
    seed = seed,
    theme = theme,
    palette = palette,
    alpha = alpha,
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Process theme
  theme <- process_theme(theme)

  # Convert to iNEXT object if needed (run iNEXT analysis)
  if (!inherits(data, "iNEXT")) {
    data <- iNEXT::iNEXT(data, ...)
  }

  # Convert iNEXT object to data frame using ggplot2::fortify
  data <- suppressWarnings({
    ggplot2::fortify(data, type = type)
  })

  # Standardize column names: Assemblage -> group, Order.q -> q
  data <- dplyr::rename(data, group = "Assemblage", q = "Order.q")

  # Auto-detect if standard error should be shown (if confidence bounds exist)
  se <- se %||% ("y.lwr" %in% names(data))

  # Create line type column: solid for rarefaction/observed, dashed for extrapolation
  data$Method2 <- data$Method
  data$Method2[data$Method2 == "Observed"] <- "Rarefaction"
  data$lty <- factor(data$Method2, levels = c("Rarefaction", "Extrapolation"))

  # Validate and process columns (convert to factors, concatenate if multiple)
  group_by <- validate_columns(
    data, group_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = group_by_sep
  )
  split_by <- validate_columns(
    data, split_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = split_by_sep
  )
  facet_by <- validate_columns(
    data, facet_by,
    force_factor = TRUE,
    allow_multi = TRUE
  )

  # Handle NULL group_by by creating a dummy group for consistent behavior
  if (is.null(group_by)) {
    group_by <- ".group"
    data$.group <- factor("")
  }

  # Collect all parameters for passing to atomic function
  params <- as.list(environment())
  params$data <- NULL # Remove data from params

  # Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = RarefactionPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Rarefaction Plot Atomic
#'
#' @description
#' Creates a single rarefaction plot without splitting
#'
#' @inheritParams RarefactionPlot
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon scale_color_manual
#' @importFrom ggplot2 scale_linetype_manual scale_shape_discrete scale_fill_manual
#' @importFrom ggplot2 element_line guide_legend unit labs
#' @importFrom rlang sym "%||%"
RarefactionPlotAtomic <- function(
    data, type = 1, se = TRUE,
    group_by = "group", group_name = NULL,
    pt_size = 3, line_width = 1,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Spectral", palcolor = NULL, alpha = 0.2,
    facet_by = NULL, facet_scales = "fixed",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    ...) {
  # Get ggplot function (with gglogger support)
  ggplot <- get_ggplot()

  # Determine axis labels based on plot type and data type
  datatype <- unique(data$datatype)
  if (type == 2L) {
    # Sample completeness curve
    if (identical(datatype, "abundance")) {
      xlab <- xlab %||% "Number of individuals"
      ylab <- ylab %||% "Sample coverage"
    } else {
      xlab <- xlab %||% "Number of sampling units"
      ylab <- ylab %||% "Sample coverage"
    }
  } else if (type == 3L || type == 4L) {
    # Coverage-based rarefaction/extrapolation curve
    xlab <- xlab %||% "Sample coverage"
    ylab <- ylab %||% "Species diversity"
  } else if (identical(datatype, "abundance")) {
    # Sample-size-based curve with abundance data
    xlab <- xlab %||% "Number of individuals"
    ylab <- ylab %||% "Species diversity"
  } else {
    # Sample-size-based curve with incidence data
    xlab <- xlab %||% "Number of sampling units"
    ylab <- ylab %||% "Species diversity"
  }

  # Set legend title (defaults to group_by column name)
  group_name <- group_name %||% group_by

  # Build base plot with points (observed data only), lines, and line types
  p <- ggplot(data, ggplot2::aes(x = !!sym("x"), y = !!sym("y"), color = !!sym(group_by))) +
    ggplot2::geom_point(
      ggplot2::aes(shape = !!sym(group_by)),
      size = pt_size,
      data = data[data$Method == "Observed", , drop = FALSE]
    ) +
    ggplot2::scale_color_manual(
      name = group_name,
      values = get_palette(levels(data[[group_by]]), palette = palette, palcolor = palcolor),
      guide = ifelse(identical(group_by, ".group"), "none", "legend")
    ) +
    ggplot2::scale_shape_discrete(
      name = group_name,
      guide = ifelse(identical(group_by, ".group"), "none", "legend")
    ) +
    ggplot2::geom_line(ggplot2::aes(linetype = !!sym("lty")), linewidth = line_width) +
    ggplot2::scale_linetype_manual(
      name = "",
      values = c("solid", "dashed"),
      guide = ggplot2::guide_legend(theme = ggplot2::theme(legend.key.width = ggplot2::unit(1, "cm")))
    )

  # Apply theme
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(
      aspect.ratio = aspect.ratio,
      panel.grid.major = ggplot2::element_line(colour = "grey80", linetype = 2)
    )

  # Apply data-driven styling (both x and y are continuous variables)
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = "x", # x is continuous (individuals/units/coverage)
    y_var = "y", # y is continuous (diversity/coverage)
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Add labels
  p <- p + ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab)

  # Add confidence intervals (ribbon) if requested
  if (isTRUE(se)) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = !!sym("y.lwr"), ymax = !!sym("y.upr"), fill = !!sym(group_by)),
        color = "transparent",
        alpha = alpha
      ) +
      ggplot2::scale_fill_manual(
        name = group_name,
        values = get_palette(levels(data[[group_by]]), palette = palette, palcolor = palcolor),
        guide = ifelse(identical(group_by, ".group"), "none", "legend")
      )
  }

  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- add_facets(
      p,
      facet_by = facet_by,
      facet_scales = facet_scales,
      nrow = facet_nrow,
      ncol = facet_ncol,
      byrow = facet_byrow
    )
  }

  return(p)
}
