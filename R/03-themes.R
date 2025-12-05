#' ggforge Theming System
#'
#' @description
#' A flexible and elegant theming system for ggforge plots
#'
#' @name themes
NULL

#' Main ggforge Theme
#'
#' @description
#' The default theme for ggforge, providing a clean and modern appearance
#'
#' @param aspect.ratio Aspect ratio of the plot panel
#' @param base_size Base font size (scales all text elements)
#' @param font_family Font family for all text
#' @param ... Additional arguments passed to \code{\link[ggplot2]{theme}}
#' @return A ggplot2 theme object
#' @export
#' @importFrom ggplot2 theme element_text element_rect element_blank element_line margin unit
#' @importFrom methods formalArgs
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_ggforge()
theme_ggforge <- function(
    aspect.ratio = NULL,
    base_size = NULL,
    font_family = NULL,
    ...) {
  # Get defaults from options
  base_size <- base_size %||% ggforge_option("theme.base_size")
  font_family <- font_family %||% ggforge_option("theme.font_family")

  # Calculate size scale
  text_size_scale <- base_size / 12

  # Start with theme_classic as base
  base_theme <- ggplot2::theme_classic(base_line_size = 0.6)

  # Custom theme arguments to override
  theme_args <- list(
    aspect.ratio = aspect.ratio,
    text = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      color = "black"
    ),
    # Title: 13号，居中，加粗
    plot.title = ggplot2::element_text(
      size = 13 * text_size_scale,
      family = font_family,
      colour = "black",
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      hjust = 0.5,
      margin = ggplot2::margin(b = 3),
      colour = "black"
    ),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin = ggplot2::margin(10, 10, 10, 10),

    # Axis titles: 12号，加粗，black
    axis.title = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      colour = "black",
      face = "bold"
    ),
    # Axis text: 默认12号（会在原子函数中动态调整）
    axis.text = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      colour = "black",
      face = "plain"
    ),

    # Strips (facets): 12号，加粗，black
    strip.text = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      colour = "black",
      hjust = 0.5,
      face = "bold",
      margin = ggplot2::margin(3, 3, 3, 3)
    ),
    strip.background = ggplot2::element_rect(fill = "grey90", colour = "black", linewidth = 0.5),

    # Legend
    legend.title = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      colour = "black",
      hjust = 0,
      face = "bold"
    ),
    legend.text = ggplot2::element_text(
      size = 10 * text_size_scale,
      family = font_family,
      colour = "black"
    ),
    legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.key.size = grid::unit(12, "pt"),
    legend.key.spacing.y = grid::unit(2, "pt"),
    legend.spacing.y = grid::unit(1, "pt"),
    legend.background = ggplot2::element_blank()
  )

  # Merge with user arguments
  user_args <- list(...)
  theme_args <- modifyList(theme_args, user_args)

  # Keep only valid theme arguments
  valid_args <- theme_args[names(theme_args) %in% methods::formalArgs(ggplot2::theme)]

  # Combine base theme with custom overrides
  base_theme + do.call(ggplot2::theme, valid_args)
}

#' ggforge Theme for Grid-based Plots (Heatmap-like)
#'
#' @description
#' A theme based on theme_bw, designed for grid-based plots like heatmaps,
#' dot plots, and correlation matrices
#'
#' @param aspect.ratio Aspect ratio of the plot panel
#' @param base_size Base font size (scales all text elements)
#' @param font_family Font family for all text
#' @param ... Additional arguments passed to \code{\link[ggplot2]{theme}}
#' @return A ggplot2 theme object
#' @export
#' @importFrom ggplot2 theme_bw theme element_text element_rect element_blank element_line margin unit
#' @importFrom methods formalArgs
theme_ggforge_grid <- function(
    aspect.ratio = NULL,
    base_size = NULL,
    font_family = NULL,
    ...) {
  # Get defaults from options
  base_size <- base_size %||% ggforge_option("theme.base_size")
  font_family <- font_family %||% ggforge_option("theme.font_family")

  # Calculate size scale
  text_size_scale <- base_size / 12

  # Start with theme_bw as base
  base_theme <- ggplot2::theme_bw(base_line_size = 0.6, base_rect_size = 1.2)

  # Custom theme arguments to override
  theme_args <- list(
    aspect.ratio = aspect.ratio,
    text = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      color = "black"
    ),
    # Title: 13号，居中，加粗
    plot.title = ggplot2::element_text(
      size = 13 * text_size_scale,
      family = font_family,
      colour = "black",
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      hjust = 0.5,
      margin = ggplot2::margin(b = 3),
      colour = "black"
    ),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin = ggplot2::margin(10, 10, 10, 10),

    # Axis titles: 12号，加粗，black
    axis.title = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      colour = "black",
      face = "bold"
    ),
    # Axis text: 默认12号（会在原子函数中动态调整）
    axis.text = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      colour = "black",
      face = "plain"
    ),

    # Strips (facets): 12号，加粗，black
    strip.text = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      colour = "black",
      hjust = 0.5,
      face = "bold",
      margin = ggplot2::margin(3, 3, 3, 3)
    ),
    strip.background = ggplot2::element_rect(fill = "grey90", colour = "black", linewidth = 0.5),

    # Legend
    legend.title = ggplot2::element_text(
      size = 12 * text_size_scale,
      family = font_family,
      colour = "black",
      hjust = 0,
      face = "bold"
    ),
    legend.text = ggplot2::element_text(
      size = 11 * text_size_scale,
      family = font_family,
      colour = "black"
    ),
    legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.key.size = grid::unit(12, "pt"),
    legend.key.spacing.y = grid::unit(2, "pt"),
    legend.spacing.y = grid::unit(1, "pt"),
    legend.background = ggplot2::element_blank()
  )

  # Merge with user arguments
  user_args <- list(...)
  theme_args <- modifyList(theme_args, user_args)

  # Keep only valid theme arguments
  valid_args <- theme_args[names(theme_args) %in% methods::formalArgs(ggplot2::theme)]

  # Combine base theme with custom overrides
  base_theme + do.call(ggplot2::theme, valid_args)
}

#' Minimal Theme
#'
#' @description
#' A minimal theme with coordinate axes
#'
#' @param add_coord Whether to add coordinate arrows
#' @param xlen_npc Length of x-axis arrow (in npc units)
#' @param ylen_npc Length of y-axis arrow (in npc units)
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param lab_size Label size
#' @param ... Additional arguments passed to theme
#' @return List of ggplot2 theme components
#' @export
#' @importFrom ggplot2 theme element_blank margin annotation_custom coord_cartesian
#' @importFrom grid grobTree gList linesGrob textGrob arrow gpar unit
theme_minimal_axes <- function(
    add_coord = TRUE,
    xlen_npc = 0.15,
    ylen_npc = 0.15,
    xlab = "",
    ylab = "",
    lab_size = 12,
    ...) {
  theme_args <- list(
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.box.margin = ggplot2::margin(0, 0, 0, 0),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.key.size = grid::unit(15, "pt"),
    legend.key.spacing.y = grid::unit(4, "pt"),
    legend.spacing.y = grid::unit(2, "pt"),
    plot.margin = ggplot2::margin(
      lab_size + 2,
      lab_size + 2,
      lab_size + 2,
      lab_size + 2,
      unit = "points"
    )
  )

  # Merge with user args
  user_args <- list(...)
  theme_args <- modifyList(theme_args, user_args)
  valid_args <- theme_args[names(theme_args) %in% methods::formalArgs(ggplot2::theme)]

  out <- do.call(ggplot2::theme, valid_args)

  if (add_coord) {
    g <- grid::grobTree(grid::gList(
      grid::linesGrob(
        x = grid::unit(c(0, xlen_npc), "npc"),
        y = grid::unit(c(0, 0), "npc"),
        arrow = grid::arrow(length = grid::unit(0.02, "npc")),
        gp = grid::gpar(lwd = 2)
      ),
      grid::textGrob(
        label = xlab,
        x = grid::unit(0, "npc"),
        y = grid::unit(0, "npc"),
        vjust = 4 / 3,
        hjust = 0,
        gp = grid::gpar(fontsize = lab_size)
      ),
      grid::linesGrob(
        x = grid::unit(c(0, 0), "npc"),
        y = grid::unit(c(0, ylen_npc), "npc"),
        arrow = grid::arrow(length = grid::unit(0.02, "npc")),
        gp = grid::gpar(lwd = 2)
      ),
      grid::textGrob(
        label = ylab,
        x = grid::unit(0, "npc"),
        y = grid::unit(0, "npc"),
        vjust = -2 / 3,
        hjust = 0,
        rot = 90,
        gp = grid::gpar(fontsize = lab_size)
      )
    ))

    return(list(
      ggplot2::annotation_custom(g),
      theme_ggforge() + out,
      ggplot2::coord_cartesian(clip = "off")
    ))
  } else {
    return(list(theme_ggforge() + out))
  }
}

#' Process theme argument
#'
#' @description
#' Converts theme name string to theme function
#'
#' @param theme Theme name or function
#' @return Theme function
#' @keywords internal
#' @importFrom utils getFromNamespace
process_theme <- function(theme) {
  if (is.function(theme)) {
    return(theme)
  }

  if (!is.character(theme)) {
    stop("Theme must be a character string or function", call. = FALSE)
  }

  # Handle namespace notation (e.g., "ggplot2::theme_minimal")
  if (grepl("::", theme)) {
    parts <- strsplit(theme, "::")[[1]]
    if (length(parts) != 2) {
      stop("Invalid theme specification: ", theme, call. = FALSE)
    }
    return(getFromNamespace(parts[2], parts[1]))
  }

  # Try to get from ggforge namespace first
  if (exists(theme, mode = "function", envir = asNamespace("ggforge"))) {
    return(get(theme, envir = asNamespace("ggforge")))
  }

  # Try ggplot2 namespace
  if (exists(theme, mode = "function", envir = asNamespace("ggplot2"))) {
    return(get(theme, envir = asNamespace("ggplot2")))
  }

  stop("Cannot find theme: ", theme, call. = FALSE)
}

# =============================================================================
# STYLE APPLICATION SYSTEM
# =============================================================================

#' Apply data-driven styling to plot
#'
#' @description
#' Automatically applies axis and legend styling based on variable types.
#' This is the core function that eliminates the need for manual
#' get_axis_text_size() calls in every plot function.
#'
#' @param plot ggplot object
#' @param data Data frame used in the plot
#' @param x_var Character. X variable name (NULL if not applicable)
#' @param y_var Character. Y variable name (NULL if not applicable)
#' @param flip Logical. Whether axes are flipped
#' @param base_size Numeric. Base font size
#' @param ... Additional theme arguments to merge
#' @return ggplot object with styling applied
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = factor(cyl))) +
#'   ggplot2::geom_point()
#'
#' # Automatic styling based on variable types
#' p <- apply_style_theme(p, mtcars, x_var = "mpg", y_var = "cyl")
#' }
apply_style_theme <- function(
    plot,
    data,
    x_var = NULL,
    y_var = NULL,
    flip = FALSE,
    base_size = 12,
    ...) {
  # Detect variable types
  x_type <- detect_var_type(data, x_var)
  y_type <- detect_var_type(data, y_var)

  # Map types to style specs
  x_spec_path <- map_type_to_style(x_type)
  y_spec_path <- map_type_to_style(y_type)

  # Handle flip
  if (flip) {
    temp <- x_spec_path
    x_spec_path <- y_spec_path
    y_spec_path <- temp
  }

  # Build theme elements
  theme_updates <- list()

  if (!is.null(x_spec_path)) {
    theme_updates$axis.text.x <- build_element_text(x_spec_path, base_size)
  }

  if (!is.null(y_spec_path)) {
    theme_updates$axis.text.y <- build_element_text(y_spec_path, base_size)
  }

  # Apply legend styling
  theme_updates$legend.title <- build_element_text("font.legend_title", base_size)
  theme_updates$legend.text <- build_element_text("font.legend_text", base_size)

  # Merge with additional arguments
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    theme_updates <- utils::modifyList(theme_updates, extra_args)
  }

  # Apply to plot
  plot + do.call(ggplot2::theme, theme_updates)
}

#' Map variable type to style specification path
#'
#' @param type Character. Variable type from detect_var_type()
#' @return Character. Style spec path
#' @keywords internal
map_type_to_style <- function(type) {
  if (is.null(type)) {
    return(NULL)
  }

  switch(type,
    continuous = "font.axis_text.continuous",
    discrete = "font.axis_text.discrete",
    ordered = "font.axis_text.discrete",
    temporal = "font.axis_text.continuous",
    NULL
  )
}
