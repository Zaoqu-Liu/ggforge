#' QQ Plot
#'
#' @description
#' QQ plot is a graphical tool to compare two distributions by plotting their
#' quantiles against each other. Can also create PP (probability-probability) plots.
#'
#' @inheritParams parameters
#' @param val A character string of the column name for the values to plot.
#'   A numeric column is expected.
#' @param val_trans A function to transform the values before plotting.
#'   Default is NULL, which means no transformation.
#' @param type A character string to specify the type of plot.
#'   Default is "qq", which means QQ plot. Other option is "pp" for PP plot.
#' @param band A list of arguments to pass to [qqplotr::stat_qq_band()] or
#'   [qqplotr::stat_pp_band()], depending on the value of `type`.
#'   Default is NULL, which means no band. If an empty list or TRUE is provided,
#'   the default arguments will be used. Multiple bands can be added by providing
#'   a list of lists.
#' @param line A list of arguments to pass to [qqplotr::stat_qq_line()] or
#'   [qqplotr::stat_pp_line()], depending on the value of `type`.
#'   Default is `list()`, which means to add a line with default arguments.
#'   If `NULL` is provided, no line will be added.
#' @param point A list of arguments to pass to [qqplotr::stat_qq_point()] or
#'   [qqplotr::stat_pp_point()], depending on the value of `type`.
#'   Default is `list()`, which means to add points with default arguments.
#'   If `NULL` is provided, no points will be added (not recommended).
#' @param fill_name A character string to name the legend of fill.
#'   Default is "Bands".
#' @param band_alpha A numeric value to set the alpha of all bands.
#'   Default is 0.5. It is a shortcut for setting alpha of all bands.
#'   You can override it by setting `alpha` in `band` argument.
#' @param xlim A numeric vector of length 2 to set the x-axis limits.
#' @param ylim A numeric vector of length 2 to set the y-axis limits.
#'
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' set.seed(8525)
#' data <- data.frame(norm = rnorm(100))
#'
#' QQPlot(data, val = "norm", band = TRUE)
#' QQPlot(data, val = "norm", band = list(
#'   list(bandType = "ks", mapping = ggplot2::aes(fill = "KS"), alpha = 0.3),
#'   list(bandType = "ts", mapping = ggplot2::aes(fill = "TS")),
#'   list(bandType = "pointwise", mapping = ggplot2::aes(fill = "Normal")),
#'   list(bandType = "boot", mapping = ggplot2::aes(fill = "Bootstrap"))
#' ), band_alpha = 0.6)
#'
#' \dontrun{
#' data(airquality, package = "datasets")
#' di <- "exp" # exponential distribution
#' dp <- list(rate = 2) # exponential rate parameter
#' QQPlot(airquality,
#'   val = "Ozone",
#'   band = list(distribution = di, dparams = dp),
#'   line = list(distribution = di, dparams = dp),
#'   point = list(distribution = di, dparams = dp)
#' )
#'
#' de <- TRUE # enabling the detrend option
#' QQPlot(airquality,
#'   val = "Ozone",
#'   band = list(distribution = di, dparams = dp, detrend = de),
#'   line = list(distribution = di, dparams = dp, detrend = de),
#'   point = list(distribution = di, dparams = dp, detrend = de)
#' )
#'
#' QQPlot(data, val = "norm", type = "pp", band = TRUE)
#'
#' dp <- list(mean = 2, sd = 2) # shifted and rescaled Normal parameters
#' QQPlot(data,
#'   val = "norm", type = "pp",
#'   band = list(dparams = dp),
#'   point = list(dparams = dp)
#' )
#'
#' QQPlot(data,
#'   val = "norm", type = "pp", band = TRUE,
#'   line = list(ab = c(.2, .5))
#' )
#'
#' di <- "exp"
#' dp <- list(rate = .022) # value is based on some empirical tests
#' de <- TRUE
#' QQPlot(airquality,
#'   val = "Ozone", type = "pp",
#'   band = list(distribution = di, detrend = de, dparams = dp),
#'   line = list(detrend = de),
#'   point = list(distribution = di, detrend = de, dparams = dp),
#'   ylim = c(-.5, .5)
#' )
#' }
QQPlot <- function(
    data, val, val_trans = NULL, type = c("qq", "pp"),
    band = NULL, line = list(), point = list(),
    fill_name = "Bands", band_alpha = 0.5,
    split_by = NULL,
    split_by_sep = "_",
    facet_by = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlim = NULL,
    ylim = NULL,
    xlab = NULL,
    ylab = NULL,
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
  # Match type argument
  type <- match.arg(type)

  # Set default axis labels based on type
  if (is.null(xlab)) {
    xlab <- if (type == "qq") "Theoretical Quantiles" else "Probability Points"
  }
  if (is.null(ylab)) {
    ylab <- if (type == "qq") "Sample Quantiles" else "Cumulative Probability"
  }

  # Validate common arguments
  validate_common_args(
    seed = seed,
    facet_by = facet_by,
    split_by = split_by,
    theme = theme,
    palette = palette,
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Validate specific QQ plot arguments
  stopifnot(
    "'band' must be TRUE, a list or NULL" = isTRUE(band) || is.list(band) || is.null(band),
    "'line' must be a list or NULL" = is.list(line) || is.null(line),
    "'point' must be a list or NULL" = is.list(point) || is.null(point),
    "'xlim' must be a numeric vector of length 2 or NULL" = is.null(xlim) || (is.numeric(xlim) && length(xlim) == 2),
    "'ylim' must be a numeric vector of length 2 or NULL" = is.null(ylim) || (is.numeric(ylim) && length(ylim) == 2)
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
    atomic_fn = QQPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' QQ Plot Atomic
#'
#' @description
#' Creates a single QQ or PP plot without splitting
#'
#' @inheritParams QQPlot
#' @keywords internal
#' @importFrom ggplot2 aes labs theme element_line scale_fill_manual waiver xlim ylim
#' @importFrom rlang sym eval_tidy as_name "%||%"
#' @importFrom utils modifyList
QQPlotAtomic <- function(
    data, val, val_trans = NULL, type = "qq",
    band = NULL, line = list(), point = list(),
    fill_name = "Bands", band_alpha = 0.5,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlim = NULL,
    ylim = NULL,
    xlab = NULL,
    ylab = NULL,
    seed = 8525,
    ...) {
  # Set seed
  set.seed(seed)

  # Validate val column
  val <- validate_columns(data, val)

  # Get ggplot function (support gglogger)
  ggplot <- get_ggplot()

  # Apply transformation if provided
  if (!is.null(val_trans)) {
    data[[val]] <- val_trans(data[[val]])
  }

  # Initialize plot
  p <- ggplot(data, ggplot2::aes(sample = !!rlang::sym(val)))

  # Track band names for legend control
  bands <- c()

  # Add confidence bands if requested
  if (!is.null(band)) {
    band_fn <- if (type == "qq") {
      qqplotr::stat_qq_band
    } else {
      qqplotr::stat_pp_band
    }

    # Convert TRUE to empty list
    if (isTRUE(band)) {
      band <- list()
    }

    # Ensure band is a list of lists
    if (length(band) == 0 || !is.null(names(band))) {
      # single band
      band <- list(band)
    }

    # Add each band
    for (i in seq_along(band)) {
      bnd <- band[[i]]

      # Assign default fill aesthetic based on band number
      if (i == 1) {
        default_bnd_fill <- ggplot2::aes(fill = "Band_1")
      } else if (i == 2) {
        default_bnd_fill <- ggplot2::aes(fill = "Band_2")
      } else if (i == 3) {
        default_bnd_fill <- ggplot2::aes(fill = "Band_3")
      } else if (i == 4) {
        default_bnd_fill <- ggplot2::aes(fill = "Band_4")
      } else if (i == 5) {
        default_bnd_fill <- ggplot2::aes(fill = "Band_5")
      } else if (i == 6) {
        default_bnd_fill <- ggplot2::aes(fill = "Band_6")
      } else if (i == 7) {
        default_bnd_fill <- ggplot2::aes(fill = "Band_7")
      } else if (i == 8) {
        default_bnd_fill <- ggplot2::aes(fill = "Band_8")
      } else if (i == 9) {
        default_bnd_fill <- ggplot2::aes(fill = "Band_9")
      } else if (i == 10) {
        default_bnd_fill <- ggplot2::aes(fill = "Band_10")
      } else {
        stop("[QQPlot] Too many bands! Please specify the fill aesthetic manually.",
          call. = FALSE
        )
      }

      # Set mapping
      bnd$mapping <- bnd$mapping %||% default_bnd_fill
      if (is.null(bnd$mapping$fill)) {
        bnd$mapping <- utils::modifyList(bnd$mapping, default_bnd_fill)
      }

      # Set alpha
      bnd$alpha <- bnd$alpha %||% band_alpha

      # Track band name
      bands <- c(bands, rlang::as_name(rlang::eval_tidy(bnd$mapping)$fill))

      # Add band to plot
      p <- p + do.call(band_fn, bnd)
    }
  }

  # Determine legend position
  if (length(bands) == 0 || all(startsWith(bands, "Band_"))) {
    legend.position <- if (inherits(legend.position, "waiver")) {
      "none"
    } else {
      legend.position
    }
  } else {
    legend.position <- if (inherits(legend.position, "waiver")) {
      "right"
    } else {
      legend.position
    }
  }

  # Add reference line if requested
  if (!is.null(line)) {
    line_fn <- if (type == "qq") {
      qqplotr::stat_qq_line
    } else {
      qqplotr::stat_pp_line
    }
    p <- p + do.call(line_fn, line)
  }

  # Add points if requested
  if (!is.null(point)) {
    point_fn <- if (type == "qq") {
      qqplotr::stat_qq_point
    } else {
      qqplotr::stat_pp_point
    }
    p <- p + do.call(point_fn, point)
  }

  # Add fill scale if bands exist
  if (length(bands) > 0) {
    p <- p +
      ggplot2::scale_fill_manual(
        name = fill_name,
        values = get_palette(bands, palette = palette, palcolor = palcolor)
      )
  }

  # Apply axis limits if specified
  if (!is.null(xlim)) {
    p <- p + ggplot2::xlim(xlim[1], xlim[2])
  }
  if (!is.null(ylim)) {
    p <- p + ggplot2::ylim(ylim[1], ylim[2])
  }

  # Apply labels
  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% val,
      y = ylab %||% val
    )

  # Apply base theme with custom grid styling for QQ/PP plots
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(
      aspect.ratio = aspect.ratio,
      panel.grid.major = ggplot2::element_line(colour = "grey80", linetype = 2)
    )

  # Apply data-driven styling (both axes are continuous for QQ/PP plots)
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = val,
    y_var = val,
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Add faceting if requested
  p <- add_facets(
    p,
    facet_by = facet_by,
    facet_scales = facet_scales,
    nrow = facet_nrow,
    ncol = facet_ncol,
    byrow = facet_byrow
  )

  return(p)
}
