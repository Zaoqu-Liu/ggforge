#' Spatial Plotting Functions for ggforge
#'
#' @description
#' Functions for plotting spatial data including raster images, masks, shapes, and points.
#' These functions work with `terra` SpatRaster and SpatVector objects, as well as regular
#' data frames.
#'
#' @name spatialplots
#' @keywords spatial
NULL

#' Flip y-coordinates for spatial data
#'
#' @description
#' Internal functions to flip y-coordinates of spatial objects.
#' This is useful for visualizing spatial data with the origin at the top left corner.
#'
#' @param data A SpatRaster, SpatVector, or data.frame object
#' @param y Column name for y-coordinates (data.frame only)
#' @param ... Additional arguments
#' @return Object with flipped y-coordinates
#' @keywords internal
#' @rdname dot_flip_y
.flip_y <- function(data, ...) {
  UseMethod(".flip_y", data)
}

#' @keywords internal
#' @rdname dot_flip_y
.flip_y.SpatRaster <- function(data, ...) {
  out <- terra::flip(data, direction = "vertical")
  nrows <- terra::nrow(data)
  ext_orig <- terra::ext(data)
  new_ext <- terra::ext(
    ext_orig[1], ext_orig[2],
    -1 * ext_orig[4], -1 * ext_orig[3]
  )
  terra::ext(out) <- new_ext
  return(out)
}

#' @keywords internal
#' @rdname dot_flip_y
.flip_y.SpatVector <- function(data, ...) {
  coords_list <- terra::geom(data)
  coords_list[, "y"] <- -coords_list[, "y"]
  data_flipped <- terra::vect(coords_list, type = terra::geomtype(data), atts = terra::values(data))
  return(data_flipped)
}

#' @keywords internal
#' @rdname dot_flip_y
.flip_y.data.frame <- function(data, y = "y", ...) {
  data[[y]] <- -data[[y]]
  return(data)
}

#' Prepare extent for spatial plots
#'
#' @param ext A numeric vector of length 4 or a SpatExtent object
#' @return A SpatExtent object or NULL
#' @keywords internal
.prepare_extent <- function(ext) {
  if (is.null(ext)) {
    return(NULL)
  }
  if (is.numeric(ext) && length(ext) == 4) {
    ext <- terra::ext(ext[1], ext[2], ext[3], ext[4])
  }
  if (!inherits(ext, "SpatExtent")) {
    stop("'ext' must be a numeric vector of length 4 or a SpatExtent object.", call. = FALSE)
  }
  return(ext)
}

#' Wrap spatial plot layers
#'
#' @param layers List of ggplot layers
#' @param ext Spatial extent
#' @param flip_y Whether to flip y-axis
#' @param legend.position Legend position
#' @param legend.direction Legend direction
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param theme Theme function or name
#' @param theme_args Theme arguments
#' @return A ggplot object
#' @keywords internal
#' @importFrom ggplot2 coord_sf labs scale_y_continuous theme
#' @importFrom rlang "%||%"
.wrap_spatial_layers <- function(
    layers, ext = NULL, flip_y = TRUE,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    theme = "theme_ggforge", theme_args = list()) {
  # Get ggplot function with gglogger support
  ggplot <- get_ggplot()

  xlim <- ylim <- NULL
  base_height <- 6
  base_width <- 6

  if (!is.null(ext)) {
    ext_width <- ext[2] - ext[1]
    ext_height <- ext[4] - ext[3]
    aspect_ratio <- ext_width / ext_height

    base_height <- 6
    base_width <- base_height * aspect_ratio

    if (base_width > 12) {
      base_width <- 12
      base_height <- base_width / aspect_ratio
    } else if (base_width < 3) {
      base_width <- 3
      base_height <- base_width / aspect_ratio
    }

    xlim <- c(ext[1], ext[2])
    if (flip_y) {
      ylim <- c(-ext[4], -ext[3])
    } else {
      ylim <- c(ext[3], ext[4])
    }
  }

  if (!identical(legend.position, "none")) {
    if (legend.position %in% c("right", "left")) {
      base_width <- base_width + 1
    } else if (legend.direction == "horizontal") {
      base_height <- base_height + 1
    } else {
      base_width <- base_width + 2
    }
  }

  theme_fn <- process_theme(theme)

  p <- ggplot() +
    layers +
    coord_sf(expand = 0, xlim = xlim, ylim = ylim) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab
    )

  # Get base_size for dynamic text sizing
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  axis_size <- 10 * base_size / 12 # Spatial coordinates are continuous

  p <- p +
    do.call(theme_fn, theme_args) +
    ggplot2::theme(
      legend.position = legend.position,
      legend.direction = legend.direction,
      axis.text.x = ggplot2::element_text(size = axis_size, colour = "black"),
      axis.text.y = ggplot2::element_text(size = axis_size, colour = "black")
    )

  if (flip_y) {
    p <- p + scale_y_continuous(labels = function(x) sub("-", "\u2212", as.character(-x)))
  }

  attr(p, "height") <- base_height
  attr(p, "width") <- base_width

  return(p)
}

#' Adjust colors with alpha
#'
#' @param colors Color vector
#' @param alpha Alpha value
#' @return Colors with alpha applied
#' @keywords internal
#' @importFrom scales alpha
adjcolors <- function(colors, alpha) {
  if (alpha >= 1) {
    return(colors)
  }
  scales::alpha(colors, alpha)
}

#' Plot SpatRaster as image
#'
#' @param data A SpatRaster object from terra package
#' @param ext Spatial extent (SpatExtent or numeric vector of length 4)
#' @param raster Whether to rasterize (auto-detect based on size)
#' @param raster_dpi Resolution for rasterization
#' @param flip_y Whether to flip y-axis
#' @param palette Color palette name
#' @param palcolor Custom colors
#' @param palette_reverse Reverse palette
#' @param alpha Transparency level
#' @param fill_name Legend title
#' @param return_layer Whether to return layers only
#' @param theme Theme name or function
#' @param theme_args Theme arguments
#' @param legend.position Legend position
#' @param legend.direction Legend direction
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param seed Random seed
#' @return ggplot object or list of layers
#' @export
#' @rdname spatialplots
#' @importFrom ggplot2 geom_raster aes scale_fill_identity scale_fill_gradientn guide_colorbar
#' @importFrom rlang sym "%||%"
#' @importFrom scales rescale
#' @examples
#' \dontrun{
#' # Create a simple raster
#' library(terra)
#' r <- rast(ncols = 100, nrows = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
#' values(r) <- runif(ncell(r))
#'
#' # Plot raster image
#' SpatImagePlot(r, palette = "viridis")
#'
#' # Plot with custom extent
#' SpatImagePlot(r, ext = c(20, 80, 20, 80), palette = "turbo")
#' }
SpatImagePlot <- function(
    data,
    ext = NULL, raster = NULL, raster_dpi = NULL, flip_y = TRUE,
    palette = "turbo", palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL, return_layer = FALSE,
    theme = "theme_ggforge", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525) {
  set.seed(seed)
  stopifnot("'data' must be a SpatRaster object" = inherits(data, "SpatRaster"))

  ext <- .prepare_extent(ext)
  if (!is.null(ext)) {
    data <- terra::crop(data, ext)
    if (terra::ncell(data) == 0) {
      stop("[SpatImagePlot] No data in the specified extent.", call. = FALSE)
    }
  }

  raster <- raster %||% (terra::ncell(data) > 1e6)
  if (raster) {
    if (is.null(raster_dpi)) {
      raster_dpi <- c(512, 512)
    }
    if (length(raster_dpi) == 1) {
      raster_dpi <- rep(raster_dpi, 2)
    }

    current_dims <- c(terra::ncol(data), terra::nrow(data))
    agg_factors <- pmax(1, round(current_dims / raster_dpi))
    data <- terra::aggregate(data, fact = agg_factors, fun = "mean")
  }

  if (flip_y) {
    data <- .flip_y(data)
  }

  if (dim(data)[3] == 3) {
    names(data) <- c("red", "green", "blue")
    data <- terra::as.data.frame(data, xy = TRUE)
    data$red <- scales::rescale(data$red, to = c(0, 255))
    data$green <- scales::rescale(data$green, to = c(0, 255))
    data$blue <- scales::rescale(data$blue, to = c(0, 255))
    data$value <- grDevices::rgb(data$red, data$green, data$blue, maxColorValue = 255)
    colnames(data)[1:2] <- c("x", "y")
    fill_identity <- TRUE
  } else {
    data <- terra::as.data.frame(data, xy = TRUE)
    colnames(data) <- c("x", "y", "value")
    fill_identity <- FALSE
  }

  layers <- list(
    ggplot2::geom_raster(
      data = data,
      ggplot2::aes(x = !!rlang::sym("x"), y = !!rlang::sym("y"), fill = !!rlang::sym("value")),
      alpha = alpha
    )
  )

  if (fill_identity) {
    layers <- c(layers, list(ggplot2::scale_fill_identity(guide = "none")))
  } else {
    layers <- c(layers, list(
      ggplot2::scale_fill_gradientn(
        name = fill_name %||% names(data)[1],
        n.breaks = 4,
        colors = get_palette(1:256, palette = palette, palcolor = palcolor, type = "continuous", reverse = palette_reverse, keep_names = FALSE),
        guide = if (identical(legend.position, "none")) {
          "none"
        } else {
          ggplot2::guide_colorbar(
            frame.colour = "black", ticks.colour = "black",
            frame.linewidth = 0.3, ticks.linewidth = 0.3,
            title.hjust = 0, alpha = alpha
          )
        },
        na.value = "transparent"
      )
    ))
  }

  attr(layers, "scales") <- "fill"

  if (return_layer) {
    return(layers)
  }

  .wrap_spatial_layers(layers,
    ext = ext, flip_y = flip_y,
    legend.position = legend.position, legend.direction = legend.direction,
    title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
    theme = theme, theme_args = theme_args
  )
}

#' Plot SpatRaster as masks
#'
#' @param data A SpatRaster object from terra package
#' @param ext Spatial extent (SpatExtent or numeric vector of length 4)
#' @param flip_y Whether to flip y-axis
#' @param add_border Whether to add border around masks
#' @param border_color Border color
#' @param border_size Border width
#' @param border_alpha Border transparency
#' @return ggplot object or list of layers
#' @export
#' @rdname spatialplots
#' @importFrom ggplot2 geom_sf
#' @examples
#' \dontrun{
#' # Create a mask raster
#' library(terra)
#' r <- rast(ncols = 50, nrows = 50, xmin = 0, xmax = 50, ymin = 0, ymax = 50)
#' values(r) <- sample(0:3, ncell(r), replace = TRUE)
#'
#' # Plot masks with borders
#' SpatMasksPlot(r, add_border = TRUE, palette = "Set2")
#' }
SpatMasksPlot <- function(
    data,
    ext = NULL, flip_y = TRUE, add_border = TRUE, border_color = "black",
    border_size = 0.5, border_alpha = 1,
    palette = "turbo", palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL, return_layer = FALSE,
    theme = "theme_ggforge", theme_args = list(),
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525) {
  set.seed(seed)

  stopifnot("'data' must be a SpatRaster object" = inherits(data, "SpatRaster"))

  ext <- .prepare_extent(ext)
  if (!is.null(ext)) {
    data <- terra::crop(data, ext)
    if (terra::ncell(data) == 0) {
      stop("[SpatMasksPlot] No data in the specified extent.", call. = FALSE)
    }
  }

  if (flip_y) {
    data <- .flip_y(data)
  }

  data[data == 0] <- NA

  layers <- list(
    ggplot2::geom_raster(
      data = stats::setNames(terra::as.data.frame(data, xy = TRUE), c("x", "y", "value")),
      ggplot2::aes(x = !!rlang::sym("x"), y = !!rlang::sym("y"), fill = !!rlang::sym("value")),
      alpha = alpha
    )
  )

  if (add_border) {
    polys <- terra::as.polygons(data, dissolve = TRUE)
    layers <- c(
      layers,
      list(ggplot2::geom_sf(
        data = sf::st_as_sf(polys),
        fill = NA,
        color = border_color,
        linewidth = border_size,
        alpha = border_alpha
      ))
    )
  }

  layers <- c(
    layers,
    list(ggplot2::scale_fill_gradientn(
      name = fill_name %||% names(data)[1],
      n.breaks = 4,
      colors = get_palette(1:256, palette = palette, palcolor = palcolor, type = "continuous", reverse = palette_reverse, keep_names = FALSE),
      guide = if (identical(legend.position, "none")) {
        "none"
      } else {
        ggplot2::guide_colorbar(
          frame.colour = "black", ticks.colour = "black",
          frame.linewidth = 0.3, ticks.linewidth = 0.3, title.hjust = 0
        )
      },
      na.value = "transparent"
    ))
  )

  attr(layers, "scales") <- "fill"

  if (return_layer) {
    return(layers)
  }

  .wrap_spatial_layers(layers,
    ext = ext, flip_y = flip_y,
    legend.position = legend.position, legend.direction = legend.direction,
    title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
    theme = theme, theme_args = theme_args
  )
}

#' Plot spatial shapes
#'
#' @param data A SpatVector object or data.frame
#' @param x X-coordinate column (for data.frame)
#' @param y Y-coordinate column (for data.frame)
#' @param group Grouping column (for data.frame)
#' @param fill_by Column(s) to fill shapes by
#' @param highlight Rows to highlight
#' @param highlight_alpha Highlight transparency
#' @param highlight_size Highlight size
#' @param highlight_color Highlight color
#' @param highlight_stroke Highlight stroke width
#' @param facet_scales Facet scales
#' @param facet_nrow Number of facet rows
#' @param facet_ncol Number of facet columns
#' @param facet_byrow Fill facets by row
#' @return ggplot object or list of layers
#' @export
#' @rdname spatialplots
#' @examples
#' \dontrun{
#' # Create polygon data
#' library(terra)
#' v <- vect("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
#'
#' # Plot shapes
#' SpatShapesPlot(v, fill_by = "grey80", border_color = "black")
#'
#' # Plot with data.frame
#' poly_df <- data.frame(
#'   x = c(0, 10, 10, 0),
#'   y = c(0, 0, 10, 10),
#'   group = rep(1, 4)
#' )
#' SpatShapesPlot(poly_df, x = "x", y = "y", group = "group")
#' }
SpatShapesPlot <- function(
    data, x = NULL, y = NULL, group = NULL,
    ext = NULL, flip_y = TRUE,
    fill_by = NULL, border_color = "black", border_size = 0.5, border_alpha = 1,
    palette = NULL, palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL,
    highlight = NULL, highlight_alpha = 1, highlight_size = 1, highlight_color = "black", highlight_stroke = 0.8,
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    return_layer = FALSE,
    theme = "theme_ggforge", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525) {
  UseMethod("SpatShapesPlot", data)
}

#' @export
#' @rdname spatialplots
SpatShapesPlot.SpatVector <- function(
    data, x = NULL, y = NULL, group = NULL,
    ext = NULL, flip_y = TRUE,
    fill_by = NULL, border_color = "black", border_size = 0.5, border_alpha = 1,
    palette = NULL, palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL,
    highlight = NULL, highlight_alpha = 1, highlight_size = 1, highlight_color = "black", highlight_stroke = 0.8,
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    return_layer = FALSE,
    theme = "theme_ggforge", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525) {
  set.seed(seed)

  ext <- .prepare_extent(ext)
  if (!is.null(ext)) {
    data <- terra::crop(data, ext)
    if (terra::nrow(data) == 0 || terra::ncol(data) == 0) {
      stop("[SpatShapesPlot] No data in the specified extent.", call. = FALSE)
    }
  }

  if (flip_y) {
    data <- .flip_y(data)
  }

  data_sf <- sf::st_as_sf(data)

  # Handle multiple fill_by columns for faceting
  if (!is.null(fill_by) && length(fill_by) > 1) {
    missing_cols <- fill_by[!fill_by %in% names(data_sf)]
    if (length(missing_cols) > 0) {
      stop("Columns not found in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }

    non_numeric_cols <- fill_by[!sapply(fill_by, function(col) is.numeric(data_sf[[col]]))]
    if (length(non_numeric_cols) > 0) {
      stop("Multiple fill_by columns must be numeric: ", paste(non_numeric_cols, collapse = ", "), call. = FALSE)
    }

    facet_data_list <- list()
    for (col in fill_by) {
      temp_data <- data_sf
      temp_data$.fill <- temp_data[[col]]
      temp_data$.facet_var <- col
      facet_data_list[[col]] <- temp_data
    }

    data_sf <- do.call(rbind, facet_data_list)
    data_sf$.facet_var <- factor(data_sf$.facet_var, levels = fill_by)
    fill_by <- ".fill"
    facet_by <- ".facet_var"
  } else {
    if (!is.null(fill_by) && is.character(fill_by) && length(fill_by) == 1) {
      if (fill_by %in% names(data_sf)) {
        fill_by <- validate_columns(
          data_sf, fill_by,
          force_factor = !is.numeric(data_sf[[fill_by]])
        )
      }
    }
    facet_by <- NULL
  }

  # Set default palette
  if (is.null(palette) && !is.null(fill_by) && fill_by %in% names(data_sf)) {
    palette <- if (is.numeric(data_sf[[fill_by]])) "turbo" else "Paired"
  }
  palette <- palette %||% "turbo"

  aes_mapping <- ggplot2::aes()
  fill_by_is_column <- !is.null(fill_by) && is.character(fill_by) && length(fill_by) == 1 && fill_by %in% names(data_sf)

  if (fill_by_is_column) {
    aes_mapping$fill <- rlang::sym(fill_by)
  }

  border_aes <- "none"
  if (isTRUE(border_color) && fill_by_is_column) {
    aes_mapping$colour <- rlang::sym(fill_by)
    border_aes <- "same_as_fill"
  } else if (is.character(border_color) && length(border_color) == 1) {
    border_aes <- "fixed_color"
  } else if (isFALSE(border_color)) {
    border_aes <- "none"
  }

  geom_params <- list(
    data = data_sf,
    mapping = aes_mapping,
    alpha = alpha,
    linewidth = border_size
  )

  if (!("colour" %in% names(aes_mapping))) {
    if (border_aes == "fixed_color") {
      geom_params$color <- adjcolors(border_color, border_alpha)
    } else if (border_aes == "none") {
      geom_params$color <- NA
    }
  }

  if (!("fill" %in% names(aes_mapping))) {
    if (!fill_by_is_column && !is.null(fill_by)) {
      geom_params$fill <- fill_by
    }
  }

  layers <- list(do.call(ggplot2::geom_sf, geom_params))

  # Add scales
  if (fill_by_is_column) {
    if (is.numeric(data_sf[[fill_by]])) {
      layers <- c(layers, list(
        scale_fill_gradientn(
          name = fill_name %||% fill_by,
          n.breaks = 4,
          colors = get_palette(1:256, palette = palette, palcolor = palcolor, type = "continuous", reverse = palette_reverse, keep_names = FALSE),
          guide = if (identical(legend.position, "none")) "none" else guide_colorbar(frame.colour = "black", ticks.colour = "black", frame.linewidth = 0.3, ticks.linewidth = 0.3, title.hjust = 0),
          na.value = "transparent"
        )
      ))
    } else {
      layers <- c(layers, list(
        ggplot2::scale_fill_manual(
          name = fill_name %||% fill_by,
          values = get_palette(levels(data_sf[[fill_by]]), palette = palette, palcolor = palcolor, reverse = palette_reverse),
          guide = if (identical(legend.position, "none")) "none" else "legend",
          na.value = "transparent"
        )
      ))
    }
  }

  # Add color scale if border_color = TRUE
  if (border_aes == "same_as_fill") {
    if (is.numeric(data_sf[[fill_by]])) {
      layers <- c(layers, list(
        ggplot2::scale_color_gradientn(
          n.breaks = 4,
          colors = get_palette(1:256, palette = palette, palcolor = palcolor, type = "continuous", reverse = palette_reverse, keep_names = FALSE, alpha = border_alpha, transparent = FALSE),
          guide = "none",
          na.value = "transparent"
        )
      ))
    } else {
      layers <- c(layers, list(
        ggplot2::scale_color_manual(
          values = get_palette(levels(data_sf[[fill_by]]), palette = palette, palcolor = palcolor, reverse = palette_reverse, alpha = border_alpha, transparent = FALSE),
          guide = "none",
          na.value = "transparent"
        )
      ))
    }
  }

  scales_used <- c(
    if (!is.null(fill_by)) "fill",
    if (border_aes == "same_as_fill") "color"
  )

  # Add highlight
  if (!isFALSE(highlight) && !is.null(highlight)) {
    if (isTRUE(highlight)) {
      hi_sf <- data_sf
    } else if (length(highlight) == 1 && is.character(highlight)) {
      hi_sf <- dplyr::filter(data_sf, !!!rlang::parse_exprs(highlight))
    } else {
      all_inst <- rownames(data_sf) %||% 1:nrow(data_sf)
      if (!any(highlight %in% all_inst)) {
        stop("No highlight items found in data.", call. = FALSE)
      }
      if (!all(highlight %in% all_inst)) {
        warning("Not all highlight items found in data.", immediate. = TRUE)
      }
      hi_sf <- data_sf[intersect(highlight, all_inst), , drop = FALSE]
    }
    if (nrow(hi_sf) > 0) {
      layers <- c(layers, list(
        geom_sf(
          data = hi_sf,
          fill = NA,
          color = highlight_color,
          linewidth = border_size + highlight_stroke,
          alpha = highlight_alpha
        )
      ))
      scales_used <- c(scales_used, "color")
    }
  }

  attr(layers, "scales") <- unique(scales_used)

  if (return_layer) {
    return(layers)
  }

  p <- .wrap_spatial_layers(layers,
    ext = ext, flip_y = flip_y,
    legend.position = legend.position, legend.direction = legend.direction,
    title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
    theme = theme, theme_args = theme_args
  )

  if (!is.null(facet_by)) {
    p <- p + add_facets(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow)
  }

  return(p)
}

#' @export
#' @rdname spatialplots
SpatShapesPlot.data.frame <- function(
    data, x, y, group,
    ext = NULL, flip_y = TRUE,
    fill_by = "grey90", border_color = "black", border_size = 0.5, border_alpha = 1,
    palette = NULL, palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL,
    highlight = NULL, highlight_alpha = 1, highlight_size = 1, highlight_color = "black", highlight_stroke = 0.8,
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    return_layer = FALSE,
    theme = "theme_ggforge", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525) {
  set.seed(seed)

  x <- validate_columns(data, x)
  y <- validate_columns(data, y)
  group <- validate_columns(data, group, force_factor = TRUE)

  ext <- .prepare_extent(ext)
  if (!is.null(ext)) {
    data <- data[data[[x]] >= ext[1] & data[[x]] <= ext[2] & data[[y]] >= ext[3] & data[[y]] <= ext[4], , drop = FALSE]
    if (nrow(data) == 0) {
      stop("[SpatShapesPlot] No data in the specified extent.", call. = FALSE)
    }
  }

  if (flip_y) {
    data <- .flip_y(data, y = y)
  }

  # Handle multiple fill_by (similar to SpatVector method)
  if (!is.null(fill_by) && length(fill_by) > 1) {
    missing_cols <- fill_by[!fill_by %in% names(data)]
    if (length(missing_cols) > 0) {
      stop("Columns not found in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }

    non_numeric_cols <- fill_by[!sapply(fill_by, function(col) is.numeric(data[[col]]))]
    if (length(non_numeric_cols) > 0) {
      stop("Multiple fill_by columns must be numeric: ", paste(non_numeric_cols, collapse = ", "), call. = FALSE)
    }

    facet_data_list <- list()
    for (col in fill_by) {
      temp_data <- data
      temp_data$.fill <- temp_data[[col]]
      temp_data$.facet_var <- col
      facet_data_list[[col]] <- temp_data
    }

    data <- do.call(rbind, facet_data_list)
    data$.facet_var <- factor(data$.facet_var, levels = fill_by)
    fill_by <- ".fill"
    facet_by <- ".facet_var"
  } else {
    if (!is.null(fill_by) && is.character(fill_by) && length(fill_by) == 1) {
      if (fill_by %in% names(data)) {
        fill_by <- validate_columns(data, fill_by, force_factor = !is.numeric(data[[fill_by]]))
      }
    }
    facet_by <- NULL
  }

  # Set default palette
  if (is.null(palette) && !is.null(fill_by) && fill_by %in% names(data)) {
    palette <- if (is.numeric(data[[fill_by]])) "turbo" else "Paired"
  }
  palette <- palette %||% "turbo"

  aes_mapping <- ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), group = !!rlang::sym(group))
  fill_by_is_column <- !is.null(fill_by) && is.character(fill_by) && length(fill_by) == 1 && fill_by %in% names(data)

  if (fill_by_is_column) {
    aes_mapping$fill <- rlang::sym(fill_by)
  }

  border_aes <- "none"
  if (isTRUE(border_color) && fill_by_is_column) {
    aes_mapping$colour <- rlang::sym(fill_by)
    border_aes <- "same_as_fill"
  } else if (is.character(border_color) && length(border_color) == 1) {
    border_aes <- "fixed_color"
  } else if (isFALSE(border_color)) {
    border_aes <- "none"
  }

  geom_params <- list(
    data = data,
    mapping = aes_mapping,
    alpha = alpha,
    linewidth = border_size
  )

  if (!("colour" %in% names(aes_mapping))) {
    if (border_aes == "fixed_color") {
      geom_params$color <- adjcolors(border_color, border_alpha)
    } else if (border_aes == "none") {
      geom_params$color <- NA
    }
  }

  if (!("fill" %in% names(aes_mapping))) {
    if (!fill_by_is_column && !is.null(fill_by)) {
      geom_params$fill <- fill_by
    }
  }

  layers <- list(do.call(ggplot2::geom_polygon, geom_params))

  # Add scales (similar logic to SpatVector method)
  if (fill_by_is_column) {
    if (is.numeric(data[[fill_by]])) {
      layers <- c(layers, list(
        scale_fill_gradientn(
          name = fill_name %||% fill_by,
          n.breaks = 4,
          colors = get_palette(1:256, palette = palette, palcolor = palcolor, type = "continuous", reverse = palette_reverse, keep_names = FALSE),
          guide = if (identical(legend.position, "none")) "none" else guide_colorbar(frame.colour = "black", ticks.colour = "black", frame.linewidth = 0.3, ticks.linewidth = 0.3, title.hjust = 0),
          na.value = "transparent"
        )
      ))
    } else {
      layers <- c(layers, list(
        ggplot2::scale_fill_manual(
          name = fill_name %||% fill_by,
          values = get_palette(levels(data[[fill_by]]), palette = palette, palcolor = palcolor, reverse = palette_reverse),
          guide = if (identical(legend.position, "none")) "none" else "legend",
          na.value = "transparent"
        )
      ))
    }
  }

  # Add color scale if border_color = TRUE
  if (border_aes == "same_as_fill") {
    if (is.numeric(data[[fill_by]])) {
      layers <- c(layers, list(
        ggplot2::scale_color_gradientn(
          n.breaks = 4,
          colors = get_palette(1:256, palette = palette, palcolor = palcolor, type = "continuous", reverse = palette_reverse, keep_names = FALSE, alpha = border_alpha, transparent = FALSE),
          guide = "none",
          na.value = "transparent"
        )
      ))
    } else {
      layers <- c(layers, list(
        ggplot2::scale_color_manual(
          values = get_palette(levels(data[[fill_by]]), palette = palette, palcolor = palcolor, reverse = palette_reverse, alpha = border_alpha, transparent = FALSE),
          guide = "none",
          na.value = "transparent"
        )
      ))
    }
  }

  scales_used <- c(
    if (!is.null(fill_by)) "fill",
    if (border_aes == "same_as_fill") "color"
  )

  # Add highlight
  if (!isFALSE(highlight) && !is.null(highlight)) {
    if (isTRUE(highlight)) {
      hi_data <- data
    } else if (length(highlight) == 1 && is.character(highlight)) {
      hi_data <- dplyr::filter(data, !!!rlang::parse_exprs(highlight))
    } else {
      all_inst <- rownames(data) %||% 1:nrow(data)
      if (!any(highlight %in% all_inst)) {
        stop("No highlight items found in data.", call. = FALSE)
      }
      if (!all(highlight %in% all_inst)) {
        warning("Not all highlight items found in data.", immediate. = TRUE)
      }
      hi_data <- data[intersect(highlight, all_inst), , drop = FALSE]
    }
    if (nrow(hi_data) > 0) {
      layers <- c(layers, list(
        ggplot2::geom_polygon(
          data = hi_data,
          mapping = aes(x = !!sym(x), y = !!sym(y), group = !!sym(group)),
          fill = NA,
          color = highlight_color,
          linewidth = border_size + highlight_stroke,
          alpha = highlight_alpha
        )
      ))
      scales_used <- c(scales_used, "color")
    }
  }

  attr(layers, "scales") <- unique(scales_used)

  if (return_layer) {
    return(layers)
  }

  p <- .wrap_spatial_layers(layers,
    ext = ext, flip_y = flip_y,
    legend.position = legend.position, legend.direction = legend.direction,
    title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
    theme = theme, theme_args = theme_args
  )

  if (!is.null(facet_by)) {
    p <- p + add_facets(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow)
  }

  return(p)
}

#' Plot spatial points
#'
#' @param data A data.frame with spatial coordinates
#' @param x X-coordinate column name
#' @param y Y-coordinate column name
#' @param color_by Column to color points by
#' @param size_by Column to size points by
#' @param size Fixed point size (alternative to size_by)
#' @param fill_by Alternative to color_by (for compatibility)
#' @param lower_quantile Lower quantile for color scaling
#' @param upper_quantile Upper quantile for color scaling
#' @param lower_cutoff Lower cutoff for color values
#' @param upper_cutoff Upper cutoff for color values
#' @param color_name Legend title for color
#' @param size_name Legend title for size
#' @param shape Point shape
#' @param raster Whether to rasterize points
#' @param raster_dpi Rasterization resolution
#' @param hex Whether to use hex binning
#' @param hex_linewidth Hex border width
#' @param hex_count Whether to count in hex bins
#' @param hex_bins Number of hex bins
#' @param hex_binwidth Hex bin width
#' @param label Whether to add labels
#' @param label_size Label size
#' @param label_fg Label foreground color
#' @param label_bg Label background color
#' @param label_bg_r Label background ratio
#' @param label_repel Whether to repel labels
#' @param label_repulsion Label repulsion force
#' @param label_pt_size Label point size
#' @param label_pt_color Label point color
#' @param label_segment_color Label segment color
#' @param label_insitu Whether to use actual labels in situ
#' @param label_pos Label position function
#' @param graph Graph/network adjacency matrix or data
#' @param graph_x Graph x column
#' @param graph_y Graph y column
#' @param graph_xend Graph xend column
#' @param graph_yend Graph yend column
#' @param graph_value Graph value column
#' @param edge_size Edge size range
#' @param edge_alpha Edge transparency
#' @param edge_color Edge color
#' @return ggplot object or list of layers
#' @export
#' @rdname spatialplots
#' @importFrom ggplot2 geom_point geom_hex stat_summary_hex geom_segment scale_linewidth_continuous scale_size_continuous scale_color_gradientn scale_color_manual after_stat
#' @importFrom ggrepel geom_text_repel
#' @examples
#' \dontrun{
#' # Create spatial point data
#' spatial_data <- data.frame(
#'   x = runif(1000, 0, 100),
#'   y = runif(1000, 0, 100),
#'   cluster = sample(c("A", "B", "C"), 1000, replace = TRUE),
#'   value = rnorm(1000)
#' )
#'
#' # Basic point plot
#' SpatPointsPlot(spatial_data, x = "x", y = "y", color_by = "cluster")
#'
#' # Continuous color scale
#' SpatPointsPlot(spatial_data, x = "x", y = "y", color_by = "value", palette = "viridis")
#'
#' # Hex binning for large datasets
#' SpatPointsPlot(spatial_data, x = "x", y = "y", color_by = "value", hex = TRUE)
#'
#' # Add labels
#' SpatPointsPlot(spatial_data,
#'   x = "x", y = "y", color_by = "cluster",
#'   label = TRUE, label_repel = TRUE
#' )
#' }
SpatPointsPlot <- function(
    data, x = NULL, y = NULL,
    ext = NULL, flip_y = TRUE, color_by = NULL, size_by = NULL, size = NULL, fill_by = NULL,
    lower_quantile = 0, upper_quantile = 0.99, lower_cutoff = NULL, upper_cutoff = NULL,
    palette = NULL, palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, color_name = NULL, size_name = NULL, shape = 16,
    border_color = "black", border_size = 0.5, border_alpha = 1,
    raster = NULL, raster_dpi = c(512, 512),
    hex = FALSE, hex_linewidth = 0.5, hex_count = FALSE, hex_bins = 50, hex_binwidth = NULL,
    label = FALSE, label_size = 4, label_fg = "white", label_bg = "black", label_bg_r = 0.1,
    label_repel = FALSE, label_repulsion = 20, label_pt_size = 1, label_pt_color = "black",
    label_segment_color = "black", label_insitu = FALSE,
    label_pos = c("median", "mean", "max", "min", "first", "last", "center", "random"),
    highlight = NULL, highlight_alpha = 1, highlight_size = 1, highlight_color = "black", highlight_stroke = 0.8,
    graph = NULL, graph_x = NULL, graph_y = NULL, graph_xend = NULL, graph_yend = NULL, graph_value = NULL,
    edge_size = c(0.05, 0.5), edge_alpha = 0.1, edge_color = "grey40",
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    return_layer = FALSE, theme = "theme_ggforge", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525) {
  set.seed(seed)

  stopifnot("'size_by' and 'size' should not be both specified" = is.null(size_by) || is.null(size))
  stopifnot("'size' must be a single numeric value" = is.null(size) || (is.numeric(size) && length(size) == 1))
  size_by <- size_by %||% size

  stopifnot("'data' must be a data.frame" = is.data.frame(data))
  if (!is.null(fill_by) && !is.null(color_by) && fill_by != color_by) {
    stop("Can't use both 'fill_by' and 'color_by'. Always use 'color_by' for points.", call. = FALSE)
  }
  color_by <- color_by %||% fill_by

  # Auto-detect x and y columns
  if (is.null(x)) {
    x_candidates <- c("x", "X", "sdimx", "coord_x", "spatial_x", "longitude", "lon", "lng")
    x <- x_candidates[x_candidates %in% names(data)][1]
    if (is.na(x)) stop("Could not find x coordinate column. Please specify 'x' parameter.", call. = FALSE)
  }

  if (is.null(y)) {
    y_candidates <- c("y", "Y", "sdimy", "coord_y", "spatial_y", "latitude", "lat")
    y <- y_candidates[y_candidates %in% names(data)][1]
    if (is.na(y)) stop("Could not find y coordinate column. Please specify 'y' parameter.", call. = FALSE)
  }

  x <- validate_columns(data, x)
  y <- validate_columns(data, y)

  # Set up raster
  if (length(raster_dpi) == 1) {
    raster_dpi <- rep(raster_dpi, 2)
  }
  raster_is_null <- is.null(raster)
  raster <- raster %||% (nrow(data) > 1e6)
  if (isTRUE(raster) && raster_is_null) {
    warning("[SpatPointsPlot] Rasterization enabled for large dataset (nrow > 1e6). Set 'raster = FALSE' to disable.", call. = FALSE)
  }

  # Apply extent cropping
  ext <- .prepare_extent(ext)
  if (!is.null(ext)) {
    data <- data[
      data[[x]] >= ext[1] & data[[x]] <= ext[2] &
        data[[y]] >= ext[3] & data[[y]] <= ext[4], ,
      drop = FALSE
    ]
    if (nrow(data) == 0) {
      stop("[SpatPointsPlot] No data in the specified extent.", call. = FALSE)
    }
  }

  # Apply y-axis flipping
  if (flip_y) {
    data <- .flip_y(data, y = y)
  }

  # Handle multiple color_by columns for faceting
  if (!is.null(color_by) && length(color_by) > 1) {
    missing_cols <- color_by[!color_by %in% names(data)]
    if (length(missing_cols) > 0) {
      stop("Columns not found in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }

    non_numeric_cols <- color_by[!sapply(color_by, function(col) is.numeric(data[[col]]))]
    if (length(non_numeric_cols) > 0) {
      stop("Multiple color_by columns must be numeric: ", paste(non_numeric_cols, collapse = ", "), call. = FALSE)
    }

    facet_data_list <- list()
    for (col in color_by) {
      temp_data <- data
      temp_data$.color <- temp_data[[col]]
      temp_data$.facet_var <- col
      facet_data_list[[col]] <- temp_data
    }

    data <- do.call(rbind, facet_data_list)
    data$.facet_var <- factor(data$.facet_var, levels = color_by)
    color_by <- ".color"
    facet_by <- ".facet_var"
  } else {
    if (!is.null(color_by) && is.character(color_by) && length(color_by) == 1) {
      if (color_by %in% names(data)) {
        color_by <- validate_columns(data, color_by, force_factor = !is.numeric(data[[color_by]]))
      }
    }
    facet_by <- NULL
  }

  # Apply cutoffs for numeric color_by
  if (!is.null(color_by) && color_by %in% names(data) && is.numeric(data[[color_by]])) {
    lower_cutoff <- lower_cutoff %||% quantile(data[[color_by]][is.finite(data[[color_by]])], lower_quantile, na.rm = TRUE)
    upper_cutoff <- upper_cutoff %||% quantile(data[[color_by]][is.finite(data[[color_by]])], upper_quantile, na.rm = TRUE)
    if (upper_cutoff == lower_cutoff) {
      if (upper_cutoff == 0) {
        upper_cutoff <- 1e-3
      } else {
        upper_cutoff <- upper_cutoff + upper_cutoff * 1e-3
      }
    }
    data[[color_by]][data[[color_by]] < lower_cutoff] <- lower_cutoff
    data[[color_by]][data[[color_by]] > upper_cutoff] <- upper_cutoff
  }

  # Set default palette
  if (is.null(palette) && !is.null(color_by) && color_by %in% names(data)) {
    palette <- if (is.numeric(data[[color_by]])) "turbo" else "Paired"
  }
  palette <- palette %||% "turbo"

  # Check aesthetic columns
  color_by_is_column <- !is.null(color_by) && is.character(color_by) && length(color_by) == 1 && color_by %in% names(data)
  size_by_is_column <- !is.null(size_by) && is.character(size_by) && length(size_by) == 1 && size_by %in% names(data)

  scales_used <- c()
  aes_mapping <- ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))

  if (size_by_is_column && !is.null(size_by) && size_by %in% names(data)) {
    aes_mapping$size <- rlang::sym(size_by)
    scales_used <- c(scales_used, "size")
  }

  # Check if shape supports borders (shapes 21-25)
  has_border <- shape %in% 21:25

  # Handle aesthetic mapping based on shape type
  if (has_border) {
    if (color_by_is_column && !is.null(color_by) && color_by %in% names(data) && !isTRUE(raster)) {
      aes_mapping$fill <- sym(color_by)
      scales_used <- c(scales_used, "fill")

      if (isTRUE(border_color)) {
        aes_mapping$colour <- rlang::sym(color_by)
        border_aes <- "same_as_fill"
        scales_used <- c(scales_used, "color")
      } else {
        border_aes <- if (is.character(border_color)) "fixed_color" else "none"
      }
    } else {
      border_aes <- if (is.character(border_color)) "fixed_color" else "none"
    }
  } else {
    if (color_by_is_column && !is.null(color_by) && color_by %in% names(data)) {
      aes_mapping$colour <- rlang::sym(color_by)
      scales_used <- c(scales_used, "color")
    }
    border_aes <- "none"
  }

  layers <- list()

  # Add graph/network
  if (!is.null(graph)) {
    is_coord_graph <- !is.null(graph_x) && !is.null(graph_y) &&
      !is.null(graph_xend) && !is.null(graph_yend) && !is.null(graph_value)

    if (is.character(graph) && length(graph) == 1 && startsWith(graph, "@")) {
      graph <- substring(graph, 2)
      net_mat <- attr(data, graph)
    } else if (is.matrix(graph) || is.data.frame(graph)) {
      net_mat <- graph
    } else if (is.numeric(graph)) {
      graph <- colnames(data)[graph]
      net_mat <- data[graph]
    } else if (is.character(graph)) {
      net_mat <- data[graph]
    } else {
      stop("The 'graph' should be a matrix, data.frame, indexes, or column names.", call. = FALSE)
    }

    if (is_coord_graph) {
      graph_x <- validate_columns(net_mat, graph_x)
      graph_y <- validate_columns(net_mat, graph_y)
      graph_xend <- validate_columns(net_mat, graph_xend)
      graph_yend <- validate_columns(net_mat, graph_yend)
      graph_value <- validate_columns(net_mat, graph_value)
      net_mat <- data.frame(
        x = net_mat[[graph_x]], y = net_mat[[graph_y]],
        xend = net_mat[[graph_xend]], yend = net_mat[[graph_yend]],
        value = net_mat[[graph_value]]
      )
    } else {
      if (!is.matrix(net_mat)) {
        net_mat <- as.matrix(net_mat)
      }

      if (!is.null(rownames(net_mat)) && !is.null(colnames(net_mat))) {
        net_mat <- net_mat[rownames(data), rownames(data)]
      } else if (nrow(net_mat) != ncol(net_mat)) {
        stop("[SpatPointsPlot] The graph matrix should be square.", call. = FALSE)
      } else if (nrow(net_mat) != nrow(data)) {
        stop("[SpatPointsPlot] The graph matrix should have the same number of rows as the input data.", call. = FALSE)
      }
      net_mat[net_mat == 0] <- NA
      net_mat[upper.tri(net_mat)] <- NA

      handle_single_facet_value <- function(mat) {
        ndf <- reshape2::melt(mat, na.rm = TRUE, stringsAsFactors = FALSE)
        ndf$value <- as.numeric(ndf$value)
        ndf$Var1 <- as.character(ndf$Var1)
        ndf$Var2 <- as.character(ndf$Var2)
        ndf$x <- data[ndf$Var1, x]
        ndf$y <- data[ndf$Var1, y]
        ndf$xend <- data[ndf$Var2, x]
        ndf$yend <- data[ndf$Var2, y]
        return(ndf)
      }

      if (!is.null(facet_by)) {
        net_mat <- do.call(rbind, lapply(split(data, data[, facet_by]), function(d) {
          d <- handle_single_facet_value(net_mat[rownames(d), rownames(d)])
          d[, facet_by] <- d[1, facet_by]
          d
        }))
      } else {
        net_mat <- handle_single_facet_value(net_mat)
      }
    }

    layers <- c(layers, list(
      ggplot2::geom_segment(
        data = net_mat,
        mapping = ggplot2::aes(
          x = !!rlang::sym("x"), y = !!rlang::sym("y"), xend = !!rlang::sym("xend"),
          yend = !!rlang::sym("yend"), linewidth = !!rlang::sym("value")
        ),
        color = edge_color, alpha = edge_alpha, show.legend = FALSE
      ),
      ggplot2::scale_linewidth_continuous(range = edge_size)
    ))
  }

  # Create main point layers
  if (isTRUE(hex)) {
    if (is.null(color_by)) {
      stop("Hex plotting requires a 'color_by' column to aggregate values.", call. = FALSE)
    }
    if (!is.null(color_by) && color_by_is_column && !is.numeric(data[[color_by]])) {
      stop("Hex plotting only works with numeric 'color_by' values.", call. = FALSE)
    }

    if (isTRUE(hex_count)) {
      if (is.null(color_by) || !color_by_is_column) {
        stop("Don't know how to count for the hex when 'color_by' is not provided.", call. = FALSE)
      }
      geom_layer <- geom_hex(
        data = data,
        mapping = aes(
          x = !!sym(x), y = !!sym(y), color = !!sym(color_by), fill = !!sym(color_by),
          alpha = after_stat(!!sym("count"))
        ),
        linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth
      )
      scales_used <- c(scales_used, "fill", "color")
    } else {
      data_na <- data[is.na(data[[color_by]]), , drop = FALSE]
      if (nrow(data_na) > 0) {
        geom_layer <- list(
          ggplot2::geom_hex(
            data = data_na, mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y)),
            fill = "grey80", linewidth = hex_linewidth, bins = hex_bins,
            binwidth = hex_binwidth, alpha = alpha / 2
          ),
          ggplot2::stat_summary_hex(
            data = data[!is.na(data[[color_by]]), , drop = FALSE],
            mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), z = !!rlang::sym(color_by)),
            linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth, alpha = alpha
          )
        )
      } else {
        geom_layer <- ggplot2::stat_summary_hex(
          data = data,
          mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), z = !!rlang::sym(color_by)),
          linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth, alpha = alpha
        )
        scales_used <- c(scales_used, "fill")
      }
    }
  } else if (isTRUE(raster)) {
    if (raster_is_null && !identical(raster_dpi, c(512, 512))) {
      message("[SpatPointsPlot] 'raster' is enabled. Point size (size_by) is ignored, try 'raster_dpi' to control resolution.")
    }

    if (!color_by_is_column) {
      fixed_color <- if (!is.null(color_by) && is.character(color_by)) color_by else "black"
      geom_layer <- scattermore::geom_scattermore(
        data = data,
        mapping = aes(x = !!sym(x), y = !!sym(y)),
        color = fixed_color,
        alpha = alpha,
        pixels = raster_dpi
      )
    } else if (has_border) {
      geom_layer <- list(
        scattermore::geom_scattermore(
          data = data,
          mapping = aes(x = !!sym(x), y = !!sym(y)),
          color = if (border_aes == "fixed_color") adjcolors(border_color, border_alpha) else "black",
          alpha = alpha,
          pixels = raster_dpi
        ),
        scattermore::geom_scattermore(
          data = data,
          mapping = aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_by)),
          alpha = alpha,
          pixels = raster_dpi
        )
      )
      scales_used <- c(scales_used, "color")
    } else {
      geom_layer <- list(scattermore::geom_scattermore(
        data = data,
        mapping = aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_by)),
        alpha = alpha,
        pixels = raster_dpi
      ))
    }
  } else {
    geom_params <- list(
      data = data,
      mapping = aes_mapping,
      alpha = alpha,
      shape = shape
    )
    if (has_border) geom_params$stroke <- border_size

    if (!size_by_is_column && is.numeric(size_by)) {
      geom_params$size <- size_by
    }

    if (!("colour" %in% names(aes_mapping))) {
      if (has_border && border_aes == "fixed_color") {
        geom_params$color <- adjcolors(border_color, border_alpha)
      } else if (has_border && border_aes == "none") {
        geom_params$color <- NA
      } else if (!has_border && !color_by_is_column && !is.null(color_by) && is.character(color_by)) {
        geom_params$color <- color_by
      } else if (!has_border && !color_by_is_column) {
        geom_params$color <- "black"
      }
    }

    if (!("fill" %in% names(aes_mapping))) {
      if (has_border && !color_by_is_column && !is.null(color_by) && is.character(color_by)) {
        geom_params$fill <- color_by
      }
    }

    geom_layer <- do.call(ggplot2::geom_point, geom_params)
  }

  if (inherits(geom_layer, c("gg", "Layer"))) {
    layers <- c(layers, list(geom_layer))
  } else {
    layers <- c(layers, geom_layer)
  }

  legend_point_size <- 3

  # Add scales
  if (color_by_is_column && !is.null(color_by)) {
    if ("fill" %in% scales_used) {
      if (is.numeric(data[[color_by]])) {
        layers <- c(layers, list(
          scale_fill_gradientn(
            name = color_name %||% color_by,
            n.breaks = 4,
            colors = get_palette(1:256, palette = palette, palcolor = palcolor, type = "continuous", reverse = palette_reverse, keep_names = FALSE),
            guide = if (identical(legend.position, "none")) "none" else guide_colorbar(frame.colour = "black", ticks.colour = "black", frame.linewidth = 0.3, ticks.linewidth = 0.3, title.hjust = 0),
            na.value = "transparent"
          )
        ))
      } else {
        layers <- c(layers, list(
          ggplot2::scale_fill_manual(
            name = color_name %||% color_by,
            values = get_palette(levels(data[[color_by]]), palette = palette, palcolor = palcolor, reverse = palette_reverse),
            guide = if (identical(legend.position, "none")) "none" else ggplot2::guide_legend(override.aes = list(size = legend_point_size)),
            na.value = "transparent"
          )
        ))
      }
    }
    if ("color" %in% scales_used) {
      if (is.numeric(data[[color_by]])) {
        if (has_border && border_aes == "same_as_fill") {
          colors <- get_palette(1:256, palette = palette, palcolor = palcolor, type = "continuous", reverse = palette_reverse, keep_names = FALSE, alpha = border_alpha, transparent = FALSE)
        } else {
          colors <- get_palette(1:256, palette = palette, palcolor = palcolor, type = "continuous", reverse = palette_reverse, keep_names = FALSE)
        }
        layers <- c(layers, list(
          scale_color_gradientn(
            name = color_name %||% color_by,
            n.breaks = 4,
            colors = colors,
            guide = if (identical(legend.position, "none") || "fill" %in% scales_used) "none" else guide_colorbar(frame.colour = "black", ticks.colour = "black", frame.linewidth = 0.3, ticks.linewidth = 0.3, title.hjust = 0),
            na.value = "transparent"
          )
        ))
      } else {
        if (has_border && border_aes == "same_as_fill") {
          colors <- get_palette(levels(data[[color_by]]), palette = palette, palcolor = palcolor, reverse = palette_reverse, alpha = border_alpha, transparent = FALSE)
        } else {
          colors <- get_palette(levels(data[[color_by]]), palette = palette, palcolor = palcolor, reverse = palette_reverse)
        }
        layers <- c(layers, list(
          scale_color_manual(
            name = color_name %||% color_by,
            values = colors,
            guide = if (identical(legend.position, "none") || "fill" %in% scales_used) "none" else ggplot2::guide_legend(override.aes = list(size = legend_point_size)),
            na.value = "transparent"
          )
        ))
      }
    }
  }

  if ("size" %in% scales_used) {
    layers <- c(layers, list(
      scale_size_continuous(
        name = size_name %||% size_by,
        guide = if (identical(legend.position, "none")) "none" else "legend"
      )
    ))
  }

  # Add highlight
  if (!isFALSE(highlight) && !is.null(highlight)) {
    if (isTRUE(hex)) {
      stop("Highlight is not supported for hex plot.", call. = FALSE)
    }
    if (isTRUE(highlight)) {
      hi_df <- data
    } else if (length(highlight) == 1 && is.character(highlight)) {
      hi_df <- dplyr::filter(data, !!!rlang::parse_exprs(highlight))
    } else {
      all_inst <- rownames(data) %||% 1:nrow(data)
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
        layers <- c(layers, list(
          scattermore::geom_scattermore(
            data = hi_df, aes(x = !!sym(x), y = !!sym(y)), color = highlight_color,
            pointsize = floor(highlight_size) + highlight_stroke, alpha = highlight_alpha, pixels = raster_dpi
          ),
          scattermore::geom_scattermore(
            data = hi_df, aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_by)),
            pointsize = floor(highlight_size), alpha = highlight_alpha, pixels = raster_dpi
          )
        ))
        scales_used <- c(scales_used, "color")
      } else {
        layers <- c(layers, list(
          ggplot2::geom_point(
            data = hi_df, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y)), color = highlight_color,
            size = highlight_size + highlight_stroke, alpha = highlight_alpha
          ),
          ggplot2::geom_point(
            data = hi_df, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), color = !!rlang::sym(color_by)),
            size = highlight_size, alpha = highlight_alpha
          )
        ))
        scales_used <- c(scales_used, "color")
      }
    }
  }

  # Add labels
  if ((isTRUE(label_repel) || isTRUE(label_insitu)) && !isTRUE(label)) {
    message("Forcing label to be TRUE when label_repel or label_insitu is TRUE.")
    label <- TRUE
  }

  if (isTRUE(label)) {
    if (is.null(color_by) || !color_by_is_column) {
      stop("Adding labels requires 'color_by' to be specified as a categorical column.", call. = FALSE)
    }
    if (is.numeric(data[[color_by]])) {
      stop("Adding labels is not supported for numeric 'color_by' values. Use categorical data.", call. = FALSE)
    }
    if (isTRUE(hex)) {
      stop("Adding labels is not supported for hex plots.", call. = FALSE)
    }

    if (is.character(label_pos)) {
      label_pos <- match.arg(label_pos)
      if (label_pos == "median") {
        label_pos <- function(x) median(x, na.rm = TRUE)
      } else if (label_pos == "mean") {
        label_pos <- function(x) mean(x, na.rm = TRUE)
      } else if (label_pos == "first") {
        label_pos <- function(x) x[1]
      } else if (label_pos == "last") {
        label_pos <- function(x) x[length(x)]
      } else if (label_pos == "random") {
        label_pos <- function(x) sample(x, 1)
      } else if (label_pos == "center") {
        label_pos <- function(x) mean(range(x, na.rm = TRUE))
      } else if (label_pos == "min") {
        label_pos <- function(x) min(x, na.rm = TRUE)
      } else if (label_pos == "max") {
        label_pos <- function(x) max(x, na.rm = TRUE)
      } else {
        stop("Invalid label position specified.", call. = FALSE)
      }
    }

    if (!is.null(facet_by)) {
      label_df <- aggregate(data[, c(x, y)], by = list(data[[color_by]], data[[facet_by]]), FUN = label_pos)
      colnames(label_df)[1:2] <- c(".label", facet_by)
    } else {
      label_df <- aggregate(data[, c(x, y)], by = list(data[[color_by]]), FUN = label_pos)
      colnames(label_df)[1] <- ".label"
    }
    label_df <- label_df[!is.na(label_df[, ".label"]), , drop = FALSE]

    if (!isTRUE(label_insitu)) {
      label_df[, ".label"] <- seq_len(nrow(label_df))
    }

    if (isTRUE(label_repel)) {
      layers <- c(layers, list(
        ggplot2::geom_point(
          data = label_df, mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y)),
          color = label_pt_color, size = label_pt_size
        ),
        ggrepel::geom_text_repel(
          data = label_df, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), label = !!rlang::sym(".label")),
          point.size = label_pt_size, max.overlaps = 100, force = label_repulsion,
          color = label_fg, bg.color = label_bg, bg.r = label_bg_r, size = label_size, inherit.aes = FALSE
        )
      ))
    } else {
      layers <- c(layers, list(
        ggrepel::geom_text_repel(
          data = label_df, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), label = !!rlang::sym(".label")),
          fontface = "bold", min.segment.length = 0, segment.color = label_segment_color,
          point.size = NA, max.overlaps = 100, force = 0,
          color = label_fg, bg.color = label_bg, bg.r = label_bg_r, size = label_size, inherit.aes = FALSE
        )
      ))
    }
  }

  attr(layers, "scales") <- unique(scales_used)

  if (return_layer) {
    return(layers)
  }

  p <- .wrap_spatial_layers(layers,
    ext = ext, flip_y = flip_y,
    legend.position = legend.position, legend.direction = legend.direction,
    title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
    theme = theme, theme_args = theme_args
  )

  if (!is.null(facet_by)) {
    p <- p + add_facets(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow)
  }

  return(p)
}
