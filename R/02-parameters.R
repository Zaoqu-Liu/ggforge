#' Common Plot Parameters Documentation
#'
#' @description
#' This file documents common parameters used across multiple plot functions.
#' By centralizing documentation, we maintain consistency and reduce duplication.
#'
#' @name parameters
#' @param data A data frame containing the data to plot
#' @param x Column name for x-axis variable
#' @param y Column name for y-axis variable
#' @param group_by Column name(s) for grouping data
#' @param group_by_sep Separator when concatenating multiple group_by columns
#' @param split_by Column name(s) to split data into multiple plots
#' @param split_by_sep Separator when concatenating multiple split_by columns
#' @param facet_by Column name(s) for faceting the plot
#' @param facet_scales Scales for facets: "fixed", "free", "free_x", "free_y"
#' @param facet_nrow Number of rows in facet layout
#' @param facet_ncol Number of columns in facet layout
#' @param facet_byrow Fill facets by row (TRUE) or column (FALSE)
#' @param keep_empty Keep empty factor levels
#' @param theme Theme name (string) or theme function
#' @param theme_args List of arguments passed to theme function
#' @param palette Color palette name
#' @param palcolor Custom colors for palette
#' @param alpha Transparency level (0-1)
#' @param x_text_angle Angle for x-axis text labels
#' @param aspect.ratio Aspect ratio of plot panel
#' @param legend.position Legend position: "none", "left", "right", "bottom", "top"
#' @param legend.direction Legend direction: "horizontal" or "vertical"
#' @param expand Expansion values for plot axes (CSS-like padding)
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param combine Whether to combine split plots into one
#' @param nrow Number of rows when combining plots
#' @param ncol Number of columns when combining plots
#' @param byrow Fill combined plots by row
#' @param axes How to handle axes in combined plots ("keep", "collect", "collect_x", "collect_y")
#' @param axis_titles How to handle axis titles in combined plots
#' @param guides How to handle guides in combined plots ("collect", "keep", "auto")
#' @param design Custom layout design for combined plots
#' @param seed Random seed for reproducibility
#' @keywords internal
NULL

#' Plot Parameter Class
#'
#' @description
#' S3 class for storing and validating plot parameters
#'
#' @param ... Named parameters
#' @return An object of class "plot_params"
#' @keywords internal
#' @export
new_plot_params <- function(...) {
  params <- list(...)
  class(params) <- c("plot_params", "list")
  params
}

#' Merge plot parameters
#'
#' @description
#' Intelligently merge user-provided parameters with defaults
#'
#' @param user_params User-provided parameters
#' @param defaults Default parameters
#' @return Merged parameter list
#' @keywords internal
#' @importFrom utils modifyList
merge_params <- function(user_params, defaults) {
  # Remove NULL values from user_params
  user_params <- user_params[!sapply(user_params, is.null)]

  # Merge with defaults
  modifyList(defaults, user_params)
}

#' Get default plot parameters
#'
#' @description
#' Returns a list of default parameters for all plot types
#'
#' @param plot_type Optional plot type to get specific defaults
#' @return List of default parameters
#' @keywords internal
get_default_params <- function(plot_type = NULL) {
  defaults <- list(
    # Data and columns
    data = NULL,
    x = NULL,
    y = NULL,
    group_by = NULL,
    group_by_sep = "_",
    split_by = NULL,
    split_by_sep = "_",

    # Faceting
    facet_by = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,

    # Styling
    theme = ggforge_option("theme.default"),
    theme_args = list(),
    palette = ggforge_option("palette.default"),
    palcolor = NULL,
    alpha = 1,

    # Axes and labels
    x_text_angle = 0,
    aspect.ratio = 1,
    expand = NULL,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,

    # Legend
    legend.position = "right",
    legend.direction = "vertical",

    # Layout
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    axes = NULL,
    axis_titles = NULL,
    guides = NULL,
    design = NULL,

    # Other
    keep_empty = FALSE,
    seed = ggforge_option("seed")
  )

  # Add plot-type specific defaults if needed
  if (!is.null(plot_type)) {
    type_defaults <- switch(plot_type,
      scatter = list(shape = 21, size_by = 2),
      bar = list(width = 0.9, flip = FALSE),
      box = list(notch = FALSE, outlier.shape = 16),
      list()
    )
    defaults <- modifyList(defaults, type_defaults)
  }

  defaults
}
