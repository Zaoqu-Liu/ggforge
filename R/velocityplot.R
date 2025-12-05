#' Compute velocity on grid
#'
#' @description
#' Internal function to compute velocity vectors on a grid for visualization.
#' Based on scvelo's velocity_embedding_grid implementation.
#'
#' @param embedding Matrix of n_obs x n_dim with cell embedding coordinates
#' @param v_embedding Matrix of n_obs x n_dim with velocity vectors
#' @param density Density of grid points along each dimension
#' @param smooth Smoothing factor for velocity vectors
#' @param n_neighbors Number of nearest neighbors for each grid point
#' @param min_mass Minimum mass required for a grid point
#' @param scale Scaling factor for velocity vectors
#' @param adjust_for_stream Whether to adjust for streamlines
#' @param cutoff_perc Percentile cutoff for removing low-density grid points
#' @return List with x_grid and v_grid components
#' @keywords internal
#' @importFrom Matrix Matrix
#' @importFrom stats dnorm quantile
#' @importFrom rlang "%||%"
.compute_velocity_on_grid <- function(
    embedding,
    v_embedding,
    density = NULL,
    smooth = NULL,
    n_neighbors = NULL,
    min_mass = NULL,
    scale = 1,
    adjust_for_stream = FALSE,
    cutoff_perc = NULL) {
  n_obs <- nrow(embedding)
  n_dim <- ncol(embedding)

  # Set defaults
  density <- density %||% 1
  smooth <- smooth %||% 0.5
  n_neighbors <- n_neighbors %||% ceiling(n_obs / 50)
  min_mass <- min_mass %||% 1
  cutoff_perc <- cutoff_perc %||% 5

  # Create grid
  grs <- list()
  for (dim_i in 1:n_dim) {
    m <- min(embedding[, dim_i], na.rm = TRUE)
    M <- max(embedding[, dim_i], na.rm = TRUE)
    gr <- seq(m, M, length.out = ceiling(50 * density))
    grs <- c(grs, list(gr))
  }
  x_grid <- as.matrix(expand.grid(grs))

  # Compute distances
  d <- proxyC::dist(
    x = Matrix::Matrix(as.matrix(embedding), sparse = TRUE),
    y = Matrix::Matrix(x_grid, sparse = TRUE),
    method = "euclidean",
    use_nan = TRUE
  )

  # Find neighbors
  neighbors <- t(as.matrix(apply(d, 2, function(x) {
    order(x, decreasing = FALSE)[1:n_neighbors]
  })))
  dists <- t(as.matrix(apply(d, 2, function(x) {
    x[order(x, decreasing = FALSE)[1:n_neighbors]]
  })))

  # Compute weights
  weight <- stats::dnorm(
    dists,
    sd = mean(sapply(grs, function(g) g[2] - g[1])) * smooth
  )
  p_mass <- p_mass_v <- rowSums(weight)
  p_mass_v[p_mass_v < 1] <- 1

  # Compute velocity on grid
  neighbors_emb <- array(
    as.matrix(v_embedding)[neighbors, seq_len(ncol(v_embedding))],
    dim = c(dim(neighbors), dim(v_embedding)[2])
  )
  v_grid <- apply((neighbors_emb * c(weight)), c(1, 3), sum)
  v_grid <- v_grid / p_mass_v

  # Adjust for streamlines
  if (isTRUE(adjust_for_stream)) {
    x_grid <- matrix(
      c(unique(x_grid[, 1]), unique(x_grid[, 2])),
      nrow = 2,
      byrow = TRUE
    )
    ns <- floor(sqrt(length(v_grid[, 1])))
    v_grid <- array(t(v_grid), dim = c(2, ns, ns))

    mass <- sqrt(apply(v_grid**2, c(2, 3), sum))
    min_mass <- 10**(min_mass - 6)
    min_mass[min_mass > max(mass, na.rm = TRUE) * 0.9] <- max(mass, na.rm = TRUE) * 0.9
    cutoff <- array(t(mass), dim = c(ns, ns)) < min_mass

    lens <- t(apply(apply(abs(neighbors_emb), c(1, 3), mean), 1, sum))
    lens <- array(t(lens), dim = c(ns, ns))

    cutoff <- cutoff | lens < quantile(lens, cutoff_perc / 100)
    v_grid[1, , ][cutoff] <- NA
  } else {
    min_mass <- min_mass * quantile(p_mass, 0.99) / 100
    x_grid <- x_grid[p_mass > min_mass, , drop = FALSE]
    v_grid <- v_grid[p_mass > min_mass, , drop = FALSE]
    if (!is.null(scale)) {
      v_grid <- v_grid * scale
    }
  }

  return(list(x_grid = x_grid, v_grid = v_grid))
}

#' Cell Velocity Plot
#'
#' @description
#' Visualize RNA velocity vectors on cell embeddings using arrows or streamlines.
#' Supports three visualization modes: raw (cell-level arrows), grid (smoothed arrows),
#' and stream (continuous streamlines).
#'
#' @param embedding Matrix or data.frame of dimension n_obs x n_dim with cell embedding coordinates
#' @param v_embedding Matrix or data.frame of dimension n_obs x n_dim with velocity vectors
#' @param plot_type Type of plot: "raw" (cell-level arrows), "grid" (smoothed grid arrows), or "stream" (streamlines)
#' @param group_by Optional grouping variable for coloring arrows (only for plot_type = "raw")
#' @param group_name Name for the grouping legend
#' @param group_palette Palette for group colors
#' @param group_palcolor Custom colors for groups (overrides group_palette)
#' @param n_neighbors Number of nearest neighbors for grid computation
#' @param density Density of grid points (or sampling density for raw mode between 0-1)
#' @param smooth Smoothing factor for velocity vectors
#' @param scale Scaling factor for velocity vectors
#' @param min_mass Minimum mass for grid points
#' @param cutoff_perc Percentile cutoff for low-density grid points
#' @param arrow_angle Angle of arrowheads in degrees
#' @param arrow_color Color of arrow heads
#' @param arrow_alpha Transparency of arrows (raw and grid modes)
#' @param streamline_l Length of streamlines
#' @param streamline_minl Minimum length of streamlines
#' @param streamline_res Resolution of streamlines
#' @param streamline_n Number of streamlines
#' @param streamline_width Width range for streamlines
#' @param streamline_alpha Transparency of streamlines
#' @param streamline_color Fixed color for streamlines (NULL for gradient)
#' @param streamline_palette Palette for streamline gradient
#' @param streamline_palcolor Custom colors for streamline gradient
#' @param streamline_bg_color Background color for streamlines
#' @param streamline_bg_stroke Background stroke width
#' @param aspect.ratio Aspect ratio of plot
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param legend.position Legend position
#' @param legend.direction Legend direction
#' @param theme Theme name or function
#' @param theme_args List of arguments passed to theme function
#' @param return_layer Return ggplot layer instead of complete plot
#' @param seed Random seed
#'
#' @return A ggplot object or ggplot layer if return_layer = TRUE
#' @export
#' @importFrom ggplot2 ggplot geom_segment scale_color_manual scale_color_gradientn scale_size labs theme arrow unit guide_legend guide_colorbar after_stat
#' @importFrom rlang sym "%||%"
#' @importFrom metR geom_streamline
#' @importFrom reshape2 melt
#' @examples
#' \donttest{
#' # Load example data
#' data(dim_example)
#'
#' # Basic raw velocity plot
#' VelocityPlot(
#'   embedding = dim_example[, 1:2],
#'   v_embedding = dim_example[, 3:4]
#' )
#'
#' # Velocity plot with grouping
#' VelocityPlot(
#'   embedding = dim_example[, 1:2],
#'   v_embedding = dim_example[, 3:4],
#'   group_by = dim_example$clusters,
#'   group_palette = "Set2"
#' )
#'
#' # Grid-based velocity plot (smoothed arrows)
#' VelocityPlot(
#'   embedding = dim_example[, 1:2],
#'   v_embedding = dim_example[, 3:4],
#'   plot_type = "grid",
#'   density = 2
#' )
#'
#' # Streamline velocity plot
#' VelocityPlot(
#'   embedding = dim_example[, 1:2],
#'   v_embedding = dim_example[, 3:4],
#'   plot_type = "stream",
#'   streamline_n = 20
#' )
#' }
VelocityPlot <- function(
    embedding,
    v_embedding,
    plot_type = c("raw", "grid", "stream"),
    group_by = NULL,
    group_name = "Group",
    group_palette = "Paired",
    group_palcolor = NULL,
    n_neighbors = NULL,
    density = 1,
    smooth = 0.5,
    scale = 1,
    min_mass = 1,
    cutoff_perc = 5,
    arrow_angle = 20,
    arrow_color = "black",
    arrow_alpha = 1,
    streamline_l = 5,
    streamline_minl = 1,
    streamline_res = 1,
    streamline_n = 15,
    streamline_width = c(0, 0.8),
    streamline_alpha = 1,
    streamline_color = NULL,
    streamline_palette = "RdYlBu",
    streamline_palcolor = NULL,
    streamline_bg_color = "white",
    streamline_bg_stroke = 0.5,
    aspect.ratio = 1,
    title = "Cell velocity",
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    theme = "theme_ggforge",
    theme_args = list(),
    return_layer = FALSE,
    seed = 8525) {
  # Get ggplot function (support gglogger)
  ggplot <- get_ggplot()

  # Validate inputs
  if (!is.matrix(embedding) && !is.data.frame(embedding)) {
    stop("'embedding' must be a matrix or data.frame", call. = FALSE)
  }
  if (!is.matrix(v_embedding) && !is.data.frame(v_embedding)) {
    stop("'v_embedding' must be a matrix or data.frame", call. = FALSE)
  }

  embedding <- as.matrix(embedding)
  v_embedding <- as.matrix(v_embedding)

  if (nrow(embedding) != nrow(v_embedding)) {
    stop("'embedding' and 'v_embedding' must have the same number of rows", call. = FALSE)
  }
  if (ncol(embedding) != ncol(v_embedding)) {
    stop("'embedding' and 'v_embedding' must have the same number of columns", call. = FALSE)
  }

  # Validate common arguments
  validate_common_args(
    seed = seed,
    theme = theme,
    palette = group_palette,
    alpha = arrow_alpha,
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Process theme
  theme <- process_theme(theme)

  # Match plot type
  plot_type <- match.arg(plot_type)

  # Handle axis labels (use column names if available, otherwise use defaults)
  reduc_names <- colnames(embedding)
  if (is.null(reduc_names)) {
    xlab <- xlab %||% "Reduction 1"
    ylab <- ylab %||% "Reduction 2"
  } else {
    xlab <- xlab %||% reduc_names[1]
    ylab <- ylab %||% reduc_names[2]
  }

  # Set n_neighbors default (2% of cells, following scvelo convention)
  n_neighbors <- n_neighbors %||% ceiling(nrow(embedding) / 50)

  # Build velocity layer based on plot type
  if (plot_type == "raw") {
    velocity_layer <- .build_raw_layer(
      embedding, v_embedding, density, scale, group_by, group_name,
      group_palette, group_palcolor, arrow_angle, arrow_color, arrow_alpha
    )
  } else if (plot_type == "grid") {
    if (!is.null(group_by)) {
      warning("[VelocityPlot] Ignoring 'group_by', which is not supported for 'grid' plot type.", call. = FALSE)
    }
    velocity_layer <- .build_grid_layer(
      embedding, v_embedding, density, smooth, n_neighbors, min_mass, scale,
      arrow_angle, arrow_color, arrow_alpha
    )
  } else if (plot_type == "stream") {
    if (!is.null(group_by)) {
      warning("[VelocityPlot] Ignoring 'group_by', which is not supported for 'stream' plot type.", call. = FALSE)
    }
    velocity_layer <- .build_stream_layer(
      embedding, v_embedding, density, smooth, n_neighbors, min_mass,
      cutoff_perc, arrow_angle, arrow_color, streamline_l, streamline_minl,
      streamline_res, streamline_n, streamline_width, streamline_alpha,
      streamline_color, streamline_palette, streamline_palcolor,
      streamline_bg_color, streamline_bg_stroke
    )
  }

  # Return layer if requested
  if (isTRUE(return_layer)) {
    return(velocity_layer)
  }

  # Build complete plot
  p <- ggplot() +
    velocity_layer +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab)

  # Get base_size for dynamic text sizing
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  axis_size <- 10 * base_size / 12 # Both axes are continuous coordinates

  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(
      aspect.ratio = aspect.ratio,
      legend.position = legend.position,
      legend.direction = legend.direction,
      axis.text.x = ggplot2::element_text(size = axis_size, colour = "black"),
      axis.text.y = ggplot2::element_text(size = axis_size, colour = "black")
    )

  attr(p, "width") <- 6
  attr(p, "height") <- 6

  return(p)
}

#' Build raw velocity layer
#' @keywords internal
#' @importFrom ggplot2 geom_segment scale_color_manual guide_legend aes
#' @importFrom rlang sym
.build_raw_layer <- function(
    embedding, v_embedding, density, scale, group_by, group_name,
    group_palette, group_palcolor, arrow_angle, arrow_color, arrow_alpha) {
  # Sample cells if density < 1 (used for downsampling in raw mode)
  if (!is.null(density) && density > 0 && density < 1) {
    s <- ceiling(density * nrow(embedding))
    ix_choice <- sample(seq_len(nrow(embedding)), size = s, replace = FALSE)
    embedding <- embedding[ix_choice, , drop = FALSE]
    v_embedding <- v_embedding[ix_choice, , drop = FALSE]
    if (!is.null(group_by)) {
      group_by <- group_by[ix_choice]
    }
  }

  # Scale velocity vectors for better visualization
  if (!is.null(scale)) {
    v_embedding <- v_embedding * scale
  }

  # Build data frame with velocity vectors
  df <- data.frame(
    x = embedding[, 1],
    y = embedding[, 2],
    u = v_embedding[, 1],
    v = v_embedding[, 2]
  )
  # Calculate arrow lengths as percentage of plot size for proportional arrows
  df$length <- sqrt(df$u^2 + df$v^2)
  global_size <- sqrt(max(df$x, na.rm = TRUE)^2 + max(df$y, na.rm = TRUE)^2)
  df$length_perc <- df$length / global_size

  # Build layer with or without grouping
  if (!is.null(group_by)) {
    df[[group_name]] <- as.factor(group_by)

    layer <- list(
      ggplot2::geom_segment(
        data = df,
        mapping = ggplot2::aes(
          x = !!rlang::sym("x"), y = !!rlang::sym("y"),
          xend = !!rlang::sym("x") + !!rlang::sym("u"),
          yend = !!rlang::sym("y") + !!rlang::sym("v"),
          color = !!rlang::sym(group_name)
        ),
        alpha = arrow_alpha,
        arrow = if (utils::compareVersion(as.character(utils::packageVersion("ggplot2")), "4") < 0) {
          ggplot2::arrow(length = ggplot2::unit(df$length_perc, "npc"), type = "closed", angle = arrow_angle)
        } else {
          warning("[VelocityPlot] 'arrow()' in ggplot2 >= 4.0.0 does not support varying lengths. Using fixed length instead.", call. = FALSE)
          NULL
        },
        lineend = "round",
        linejoin = "mitre",
        inherit.aes = FALSE
      ),
      ggplot2::scale_color_manual(
        name = group_name,
        values = get_palette(
          df[[group_name]],
          palette = group_palette,
          palcolor = group_palcolor
        ),
        guide = ggplot2::guide_legend(
          title.hjust = 0,
          order = 1,
          override.aes = list(linewidth = 2, alpha = 1)
        )
      )
    )
    attr(layer, "scales") <- unique(c(attr(layer, "scales"), "color"))
  } else {
    layer <- list(
      ggplot2::geom_segment(
        data = df,
        ggplot2::aes(
          x = !!rlang::sym("x"), y = !!rlang::sym("y"),
          xend = !!rlang::sym("x") + !!rlang::sym("u"),
          yend = !!rlang::sym("y") + !!rlang::sym("v")
        ),
        color = arrow_color,
        alpha = arrow_alpha,
        arrow = ggplot2::arrow(length = ggplot2::unit(df$length_perc, "npc"), type = "closed", angle = arrow_angle),
        lineend = "round",
        linejoin = "mitre",
        inherit.aes = FALSE
      )
    )
  }

  return(layer)
}

#' Build grid velocity layer
#' @keywords internal
#' @importFrom ggplot2 geom_segment aes arrow unit
#' @importFrom rlang sym
.build_grid_layer <- function(
    embedding, v_embedding, density, smooth, n_neighbors, min_mass, scale,
    arrow_angle, arrow_color, arrow_alpha) {
  # Compute velocity on grid
  res <- .compute_velocity_on_grid(
    embedding, v_embedding,
    density = density,
    smooth = smooth,
    n_neighbors = n_neighbors,
    min_mass = min_mass,
    scale = scale
  )

  x_grid <- res$x_grid
  v_grid <- res$v_grid

  # Build data frame from grid velocity
  df <- data.frame(
    x = x_grid[, 1],
    y = x_grid[, 2],
    u = v_grid[, 1],
    v = v_grid[, 2]
  )
  # Calculate arrow lengths as percentage of plot size for proportional arrows
  df$length <- sqrt(df$u^2 + df$v^2)
  global_size <- sqrt(max(df$x, na.rm = TRUE)^2 + max(df$y, na.rm = TRUE)^2)
  df$length_perc <- df$length / global_size

  layer <- list(
    ggplot2::geom_segment(
      data = df,
      ggplot2::aes(
        x = !!rlang::sym("x"), y = !!rlang::sym("y"),
        xend = !!rlang::sym("x") + !!rlang::sym("u"),
        yend = !!rlang::sym("y") + !!rlang::sym("v")
      ),
      color = arrow_color,
      alpha = arrow_alpha,
      arrow = ggplot2::arrow(length = ggplot2::unit(df$length_perc, "npc"), type = "closed", angle = arrow_angle),
      lineend = "round",
      linejoin = "mitre",
      inherit.aes = FALSE
    )
  )

  return(layer)
}

#' Build stream velocity layer
#' @keywords internal
#' @importFrom ggplot2 scale_color_gradientn scale_size guide_colorbar aes after_stat
#' @importFrom rlang sym
#' @importFrom metR geom_streamline
#' @importFrom reshape2 melt
.build_stream_layer <- function(
    embedding, v_embedding, density, smooth, n_neighbors, min_mass,
    cutoff_perc, arrow_angle, arrow_color, streamline_l, streamline_minl,
    streamline_res, streamline_n, streamline_width, streamline_alpha,
    streamline_color, streamline_palette, streamline_palcolor,
    streamline_bg_color, streamline_bg_stroke) {
  # Compute velocity on grid for streamlines
  res <- .compute_velocity_on_grid(
    embedding, v_embedding,
    density = density,
    smooth = smooth,
    n_neighbors = n_neighbors,
    min_mass = min_mass,
    scale = 1,
    cutoff_perc = cutoff_perc,
    adjust_for_stream = TRUE
  )

  x_grid <- res$x_grid
  v_grid <- res$v_grid

  # Build data frame for streamlines from grid velocity field
  df <- expand.grid(x_grid[1, ], x_grid[2, ])
  colnames(df) <- c("x", "y")
  u <- reshape2::melt(t(v_grid[1, , ]))
  v <- reshape2::melt(t(v_grid[2, , ]))
  df$u <- u$value
  df$v <- v$value
  # Replace NA with 0 for streamline computation
  df[is.na(df)] <- 0

  # Build layer with or without gradient
  if (!is.null(streamline_color)) {
    layer <- list(
      metR::geom_streamline(
        data = df,
        ggplot2::aes(x = !!rlang::sym("x"), y = !!rlang::sym("y"), dx = !!rlang::sym("u"), dy = !!rlang::sym("v")),
        L = streamline_l,
        min.L = streamline_minl,
        res = streamline_res,
        n = streamline_n,
        size = max(streamline_width, na.rm = TRUE) + streamline_bg_stroke,
        color = streamline_bg_color,
        alpha = streamline_alpha,
        arrow.type = "closed",
        arrow.angle = arrow_angle,
        lineend = "round",
        linejoin = "mitre",
        inherit.aes = FALSE
      ),
      metR::geom_streamline(
        data = df,
        ggplot2::aes(x = !!rlang::sym("x"), y = !!rlang::sym("y"), dx = !!rlang::sym("u"), dy = !!rlang::sym("v")),
        L = streamline_l,
        min.L = streamline_minl,
        res = streamline_res,
        n = streamline_n,
        size = max(streamline_width, na.rm = TRUE),
        color = streamline_color,
        alpha = streamline_alpha,
        arrow.type = "closed",
        arrow.angle = arrow_angle,
        lineend = "round",
        linejoin = "mitre",
        inherit.aes = FALSE
      ),
      metR::geom_streamline(
        data = df,
        ggplot2::aes(x = !!rlang::sym("x"), y = !!rlang::sym("y"), dx = !!rlang::sym("u"), dy = !!rlang::sym("v")),
        L = streamline_l,
        min.L = streamline_minl,
        res = streamline_res,
        n = streamline_n,
        linetype = 0,
        color = arrow_color,
        arrow.type = "closed",
        arrow.angle = arrow_angle,
        lineend = "round",
        linejoin = "mitre",
        inherit.aes = FALSE
      )
    )
  } else {
    layer <- list(
      metR::geom_streamline(
        data = df,
        ggplot2::aes(x = !!rlang::sym("x"), y = !!rlang::sym("y"), dx = !!rlang::sym("u"), dy = !!rlang::sym("v")),
        L = streamline_l,
        min.L = streamline_minl,
        res = streamline_res,
        n = streamline_n,
        size = max(streamline_width, na.rm = TRUE) + streamline_bg_stroke,
        color = streamline_bg_color,
        alpha = streamline_alpha,
        arrow.type = "closed",
        arrow.angle = arrow_angle,
        lineend = "round",
        linejoin = "mitre",
        inherit.aes = FALSE
      ),
      metR::geom_streamline(
        data = df,
        ggplot2::aes(
          x = !!rlang::sym("x"), y = !!rlang::sym("y"),
          dx = !!rlang::sym("u"), dy = !!rlang::sym("v"),
          size = ggplot2::after_stat(!!rlang::sym("step")),
          color = sqrt(ggplot2::after_stat(!!rlang::sym("dx"))^2 + ggplot2::after_stat(!!rlang::sym("dy"))^2)
        ),
        L = streamline_l,
        min.L = streamline_minl,
        res = streamline_res,
        n = streamline_n,
        alpha = streamline_alpha,
        arrow = NULL,
        lineend = "round",
        linejoin = "mitre",
        inherit.aes = FALSE
      ),
      metR::geom_streamline(
        data = df,
        ggplot2::aes(x = !!rlang::sym("x"), y = !!rlang::sym("y"), dx = !!rlang::sym("u"), dy = !!rlang::sym("v")),
        L = streamline_l,
        min.L = streamline_minl,
        res = streamline_res,
        n = streamline_n,
        linetype = 0,
        color = arrow_color,
        arrow.type = "closed",
        arrow.angle = arrow_angle,
        lineend = "round",
        linejoin = "mitre",
        inherit.aes = FALSE
      ),
      ggplot2::scale_color_gradientn(
        name = "Velocity",
        n.breaks = 4,
        colors = get_palette(
          n = 100,
          palette = streamline_palette,
          palcolor = streamline_palcolor,
          type = "continuous"
        ),
        guide = ggplot2::guide_colorbar(
          frame.colour = "black",
          ticks.colour = "black",
          frame.linewidth = 0.3,
          ticks.linewidth = 0.3,
          title.hjust = 0,
          order = 1
        )
      ),
      ggplot2::scale_size(range = range(streamline_width), guide = "none")
    )
    attr(layer, "scales") <- unique(c(attr(layer, "scales"), "color"))
  }

  return(layer)
}
