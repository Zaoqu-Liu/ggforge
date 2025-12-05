#' Color Palette System for ggforge
#'
#' @description
#' A comprehensive color palette system with support for both discrete
#' and continuous palettes, custom colors, and intelligent color mapping.
#'
#' @name palettes
NULL

#' Get colors from palette
#'
#' @description
#' Main function for retrieving colors from palettes with intelligent
#' handling of discrete and continuous data.
#'
#' @param x Vector of values to map to colors (character, factor, or numeric)
#' @param n Number of colors for continuous palettes
#' @param palette Name of the palette to use
#' @param palcolor Custom colors (overrides palette)
#' @param type Type of palette: "auto", "discrete", or "continuous"
#' @param keep_names Whether to keep names on color vector
#' @param alpha Transparency level (0-1)
#' @param matched Return colors matched to x values
#' @param reverse Reverse the color order
#' @param NA_keep Include color for NA values
#' @param NA_color Color for NA values
#' @param transparent Use true transparency vs color blending
#' @return Named vector of colors
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom stats setNames
#' @importFrom rlang "%||%"
#' @importFrom scales alpha
#' @examples
#' # Discrete palette
#' get_palette(c("A", "B", "C"), palette = "Paired")
#'
#' # Continuous palette
#' get_palette(1:100, palette = "Spectral", type = "continuous")
#'
#' # Custom colors
#' get_palette(c("A", "B", "C"), palcolor = c("red", "blue", "green"))
get_palette <- function(
    x,
    n = 100,
    palette = "Paired",
    palcolor = NULL,
    type = "auto",
    keep_names = TRUE,
    alpha = 1,
    matched = FALSE,
    reverse = FALSE,
    NA_keep = FALSE,
    NA_color = "grey80",
    transparent = TRUE) {
  # Load palette list (will be added with data)
  palette_list <- get("palette_list", envir = asNamespace("ggforge"))

  # Handle missing x
  if (missing(x)) {
    x <- 1:n
    type <- "continuous"
  }

  # Handle case where palette is a named color vector (should be palcolor)
  if (length(palette) > 1 || (!is.null(names(palette)) && length(palette) == 1)) {
    if (is.null(palcolor)) {
      palcolor <- palette
    }
    palette <- "Paired" # Use default palette name
  }

  # Validate palette
  if (length(palette) == 1 && !palette %in% names(palette_list)) {
    stop(
      sprintf(
        "Palette '%s' not found. Use show_palettes() to see available palettes",
        palette
      ),
      call. = FALSE
    )
  }

  # Handle custom colors
  if (is.list(palcolor)) {
    palcolor <- unlist(palcolor)
  }

  if (all(palcolor == "")) {
    palcolor <- palette_list[[palette]]
  }

  if (is.null(palcolor) || length(palcolor) == 0) {
    palcolor <- palette_list[[palette]]
  }

  # Handle named palcolors that match x values
  if (!is.null(names(palcolor))) {
    matched_colors <- palcolor[intersect(names(palcolor), x)]

    if (length(matched_colors) < length(x) && length(matched_colors) > 0) {
      # Partial match: fill in missing with palette
      palcolor <- get_palette(
        x = x, n = n, palette = palette, palcolor = NULL,
        type = type, keep_names = TRUE, alpha = 1,
        matched = matched, reverse = reverse,
        NA_keep = NA_keep, NA_color = NA_color,
        transparent = transparent
      )
      palcolor[names(matched_colors)] <- matched_colors
      reverse <- FALSE # Already reversed if needed
    } else if (length(matched_colors) == length(x)) {
      palcolor <- matched_colors
    }
  }

  pal_n <- length(palcolor)

  # Determine type
  if (type == "auto") {
    type <- if (is.numeric(x)) "continuous" else "discrete"
  }

  # Validate type
  if (!type %in% c("discrete", "continuous")) {
    stop("'type' must be 'auto', 'discrete', or 'continuous'", call. = FALSE)
  }

  # Generate colors based on type
  if (type == "discrete") {
    color <- .get_discrete_colors(
      x, palcolor, pal_n, matched, NA_color
    )
  } else {
    color <- .get_continuous_colors(
      x, palcolor, pal_n, n, matched, NA_color
    )
  }

  # Reverse if requested
  if (reverse) {
    if (!is.null(names(color))) {
      color <- setNames(rev(color), names(color))
    } else {
      color <- rev(color)
    }
  }

  # Handle NA
  if (!NA_keep) {
    color <- color[names(color) != "NA"]
  }

  # Remove names if requested
  if (!keep_names) {
    names(color) <- NULL
  }

  # Apply alpha
  if (alpha < 1) {
    color <- .apply_alpha(color, alpha, transparent)
  }

  return(color)
}

#' Get discrete colors
#' @keywords internal
.get_discrete_colors <- function(x, palcolor, pal_n, matched, NA_color) {
  if (!is.factor(x)) {
    x <- factor(x, levels = unique(x))
  }

  n_x <- nlevels(x)

  # Check if palette is continuous type
  if (isTRUE(attr(palcolor, "type") == "continuous")) {
    color <- colorRampPalette(palcolor)(n_x)
    names(color) <- levels(x)
  } else if (!is.null(names(palcolor))) {
    # Use named colors
    color <- palcolor[intersect(names(palcolor), levels(x))]
  } else {
    # Generate colors
    if (n_x <= pal_n) {
      color <- palcolor[1:n_x]
    } else {
      color <- colorRampPalette(palcolor)(n_x)
    }
    names(color) <- levels(x)
  }

  # Add NA color if needed
  if (any(is.na(x))) {
    color <- c(color, setNames(NA_color, "NA"))
  }

  # Match to x values
  if (matched) {
    color <- color[as.character(x)]
    color[is.na(color)] <- NA_color
  }

  return(color)
}

#' Get continuous colors
#' @keywords internal
.get_continuous_colors <- function(x, palcolor, pal_n, n, matched, NA_color) {
  if (!is.numeric(x) && all(!is.na(x))) {
    stop("x must be numeric for continuous palettes", call. = FALSE)
  }

  # Handle edge cases
  if (all(is.na(x))) {
    values <- as.factor(rep(0, n))
  } else if (length(unique(na.omit(as.numeric(x)))) == 1) {
    values <- as.factor(rep(unique(na.omit(as.numeric(x))), n))
  } else {
    if (matched) {
      values <- cut(
        x,
        breaks = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1),
        include.lowest = TRUE
      )
    } else {
      values <- cut(
        1:100,
        breaks = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1),
        include.lowest = TRUE
      )
    }
  }

  n_x <- nlevels(values)

  # Generate colors
  if (n_x <= pal_n) {
    color <- palcolor[1:n_x]
  } else {
    color <- colorRampPalette(palcolor)(n_x)
  }
  names(color) <- levels(values)

  # Add NA color
  if (any(is.na(x))) {
    color <- c(color, setNames(NA_color, "NA"))
  }

  # Match to values
  if (matched) {
    if (all(is.na(x))) {
      color <- NA_color
    } else if (length(unique(na.omit(x))) == 1) {
      color <- color[as.character(unique(na.omit(x)))]
      color[is.na(color)] <- NA_color
    } else {
      color <- color[as.character(values)]
      color[is.na(color)] <- NA_color
    }
  }

  return(color)
}

#' Apply alpha to colors
#' @keywords internal
#' @importFrom grDevices col2rgb rgb
.apply_alpha <- function(colors, alpha_val, transparent) {
  if (transparent) {
    # Use true transparency
    return(scales::alpha(colors, alpha_val))
  } else {
    # Blend with white
    has_names <- !is.null(names(colors))
    color_df <- as.data.frame(col2rgb(colors) / 255)

    colors_out <- sapply(color_df, function(color) {
      # Blend with white background
      color_rgb <- color * alpha_val + c(1, 1, 1) * (1 - alpha_val)
      rgb(color_rgb[1], color_rgb[2], color_rgb[3])
    })

    if (has_names) {
      names(colors_out) <- names(colors)
    }

    return(colors_out)
  }
}

#' Show available palettes
#'
#' @description
#' Display available color palettes visually
#'
#' @param palettes Custom palette list (NULL to use built-in)
#' @param type Type of palettes to show: "discrete", "continuous", or both
#' @param index Indices of palettes to show
#' @param palette_names Specific palette names to show
#' @param return_names Return palette names instead of plotting
#' @param return_palettes Return palette colors instead of plotting
#' @return Plot, palette names, or palette colors
#' @export
#' @importFrom ggplot2 ggplot geom_col scale_fill_manual scale_x_continuous element_blank aes
show_palettes <- function(
    palettes = NULL,
    type = c("discrete", "continuous"),
    index = NULL,
    palette_names = NULL,
    return_names = TRUE,
    return_palettes = FALSE) {
  # Get palette list
  if (!is.null(palettes)) {
    palette_list <- palettes
  } else {
    palette_list <- get("palette_list", envir = asNamespace("ggforge"))
    palette_list <- palette_list[
      unlist(lapply(palette_list, function(x) {
        isTRUE(attr(x, "type") %in% type)
      }))
    ]
  }

  # Filter by index
  if (!is.null(index)) {
    index <- index[index %in% seq_along(palette_list)]
    palette_list <- palette_list[index]
  }

  # Set names if missing
  if (is.null(names(palette_list))) {
    names(palette_list) <- seq_along(palette_list)
  }

  # Filter by names
  if (!is.null(palette_names)) {
    missing_names <- setdiff(palette_names, names(palette_list))
    if (length(missing_names) > 0) {
      stop("Cannot find palettes: ", paste(missing_names, collapse = ", "), call. = FALSE)
    }
    palette_list <- palette_list[palette_names]
  }

  # Return options
  if (return_palettes) {
    return(palette_list)
  }
  if (return_names) {
    return(names(palette_list))
  }

  # Create visualization
  df <- data.frame(
    palette = rep(names(palette_list), sapply(palette_list, length)),
    color = unlist(palette_list),
    stringsAsFactors = FALSE
  )
  df$palette <- factor(df$palette, levels = rev(unique(df$palette)))
  df$color_order <- factor(seq_len(nrow(df)), levels = seq_len(nrow(df)))
  df$proportion <- as.numeric(1 / table(df$palette)[df$palette])

  p <- ggplot2::ggplot(df, aes(y = .data$palette, x = .data$proportion, fill = .data$color_order)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = df$color) +
    scale_x_continuous(expand = c(0, 0), trans = "reverse") +
    theme_ggforge(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      panel.border = element_blank()
    )

  print(p)
  invisible(names(palette_list))
}

#' Check and normalize palette argument
#' @keywords internal
check_palette <- function(palette, split_names) {
  palette <- as.list(palette)

  if (length(palette) == 0) {
    stop("'palette' must be specified", call. = FALSE)
  }

  # Special case: no split_by (split_names = "...")
  # If palette has names and doesn't match "...", it's meant for group_by
  # So just wrap it in a list and return
  if (length(split_names) == 1 && split_names[1] == "...") {
    if (!is.null(names(palette)) && !"..." %in% names(palette)) {
      # Named palette for group_by, wrap it
      result <- list(palette)
      names(result) <- "..."
      return(result)
    }
  }

  # Handle named palettes (partial naming allowed)
  if (!is.null(names(palette)) && any(nzchar(names(palette)))) {
    missing <- setdiff(split_names, names(palette))
    if (length(missing) > 0) {
      # Fill missing values with default palette
      default_palette <- if (is.null(palette[[1]]) || palette[[1]] == "") "Paired" else palette[[1]]
      for (m in missing) {
        palette[[m]] <- default_palette
      }
      message(sprintf(
        "Using palette '%s' for split_by values without explicit palette: %s",
        default_palette, paste(missing, collapse = ", ")
      ))
    }
    # Reorder to match split_names
    palette <- palette[split_names]
    return(palette)
  }

  # Replicate if needed (unnamed palettes)
  if (length(palette) == 1 && length(split_names) > 1) {
    palette <- rep(palette, length(split_names))
  }

  if (length(palette) < length(split_names)) {
    stop(
      sprintf(
        "Palette length (%d) less than split_by values (%d)",
        length(palette), length(split_names)
      ),
      call. = FALSE
    )
  }

  # Set names
  if (is.null(names(palette))) {
    names(palette)[seq_along(split_names)] <- split_names
  }

  return(palette)
}

#' Check and normalize palcolor argument
#' @keywords internal
check_palcolor <- function(palcolor, split_names) {
  if (is.null(palcolor)) {
    return(NULL)
  }

  # Convert to list if needed
  if (!is.list(palcolor)) {
    palcolor <- list(palcolor)
  }

  # Special case: no split_by (split_names = "...")
  # If palcolor has names and doesn't match "...", it's meant for group_by
  # So just wrap it in a list and return
  if (length(split_names) == 1 && split_names[1] == "...") {
    if (!is.null(names(palcolor)) && length(palcolor) > 1 && !"..." %in% names(palcolor)) {
      # Named palcolor for group_by, wrap it
      result <- list(palcolor)
      names(result) <- "..."
      return(result)
    }
  }

  # Handle special case
  if (identical(split_names, "...") && !identical(names(palcolor), "...")) {
    names(palcolor) <- split_names
  }

  # Replicate if needed
  if (length(palcolor) == 1 && length(split_names) > 1) {
    palcolor <- rep(palcolor, length(split_names))
  }

  # Set names
  if (is.null(names(palcolor))) {
    names(palcolor)[seq_along(split_names)] <- split_names
  }

  return(palcolor)
}
