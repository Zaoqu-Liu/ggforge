#' Chord Plot Atomic
#'
#' @description
#' Creates a single chord diagram without splitting. This is an internal
#' function called by \code{\link{ChordPlot}} for each split level.
#'
#' @inheritParams parameters
#' @param y Column for relationship values (if NULL, counts relationships)
#' @param from Column(s) for source nodes
#' @param from_sep Separator for concatenating multiple from columns
#' @param to Column(s) for target nodes
#' @param to_sep Separator for concatenating multiple to columns
#' @param flip Logical. Whether to flip source and target nodes
#' @param links_color Character. Color links by source ("from") or target ("to")
#' @param labels_rot Logical. Whether to rotate labels by 90 degrees
#' @param ... Additional arguments (not currently used)
#'
#' @return A patchwork-wrapped chord diagram with height/width attributes
#' @keywords internal
#' @importFrom dplyr %>% group_by summarise n select
#' @importFrom patchwork wrap_elements plot_annotation
#' @importFrom rlang sym
ChordPlotAtomic <- function(
    data,
    y = NULL,
    from = NULL,
    from_sep = "_",
    to = NULL,
    to_sep = "_",
    flip = FALSE,
    links_color = c("from", "to"),
    labels_rot = FALSE,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 0.5,
    title = NULL,
    subtitle = NULL,
    ...) {
  # Check for circlize package
  if (!requireNamespace("circlize", quietly = TRUE)) {
    stop("Package 'circlize' is required for chord plots. Install with: install.packages('circlize')", call. = FALSE)
  }

  # Validate arguments
  links_color <- match.arg(links_color)

  # Validate and process columns
  from <- validate_columns(
    data, from,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = from_sep
  )

  to <- validate_columns(
    data, to,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = to_sep
  )

  y <- validate_columns(data, y)

  # Handle NULL y: compute counts of relationships
  if (is.null(y)) {
    data <- data %>%
      dplyr::group_by(!!rlang::sym(from), !!rlang::sym(to)) %>%
      dplyr::summarise(.y = dplyr::n(), .groups = "drop")
    y <- ".y"
  }

  # Flip source and target if requested
  if (isTRUE(flip)) {
    data <- data %>% dplyr::select(from = !!rlang::sym(to), to = !!rlang::sym(from), value = !!rlang::sym(y))
  } else {
    data <- data %>% dplyr::select(from = !!rlang::sym(from), to = !!rlang::sym(to), value = !!rlang::sym(y))
  }

  # Prepare data for circlize: sort and convert to character
  data <- data[order(data$from, data$to), , drop = FALSE]
  data$from <- as.character(data$from)
  data$to <- as.character(data$to)

  # Extract unique node sets
  froms <- unique(data$from)
  tos <- unique(data$to)
  all_nodes <- unique(c(froms, tos))

  # Get colors for nodes and map to links
  grid_cols <- get_palette(all_nodes, palette = palette, palcolor = palcolor)
  link_cols <- grid_cols[data[[links_color]]]

  # Clear circlize state to avoid conflicts with previous plots
  circlize::circos.clear()
  circlize::circos.par(track.margin = c(0.01, 0.02))

  # Create chord diagram based on label rotation
  if (!isTRUE(labels_rot)) {
    # Horizontal labels
    p <- ~ {
      circlize::chordDiagram(
        data,
        grid.col = grid_cols,
        col = link_cols,
        transparency = 1 - alpha,
        direction = 1,
        annotationTrack = "grid",
        direction.type = c("diffHeight", "arrows"),
        link.arr.type = "big.arrow",
        link.arr.length = 0.04,
        preAllocateTracks = list(
          list(track.height = circlize::mm_h(1)),
          list(track.height = circlize::mm_h(0.1))
        )
      )
      circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
        circlize::circos.text(
          circlize::CELL_META$xcenter,
          circlize::CELL_META$ylim[1] + 5.5,
          circlize::CELL_META$sector.index,
          niceFacing = TRUE,
          adj = c(0.5, 0.5)
        )
      }, bg.border = NA)
      circlize::circos.track(track.index = 2, panel.fun = function(x, y) {
        for (si in circlize::get.all.sector.index()) {
          start.degree <- circlize::get.cell.meta.data("cell.start.degree", sector.index = si)
          end.degree <- circlize::get.cell.meta.data("cell.end.degree", sector.index = si)
          if (abs(end.degree - start.degree) > 2) {
            circlize::circos.axis(
              h = "top",
              labels.cex = 0.7,
              labels.niceFacing = TRUE,
              sector.index = si
            )
          }
        }
      }, bg.border = NA)
    }
  } else {
    # Rotated labels
    p <- ~ {
      circlize::chordDiagram(
        data,
        grid.col = grid_cols,
        col = link_cols,
        transparency = 1 - alpha,
        direction = 1,
        annotationTrack = "grid",
        direction.type = c("diffHeight", "arrows"),
        link.arr.type = "big.arrow",
        link.arr.length = 0.04,
        preAllocateTracks = list(
          list(track.height = max(strwidth(all_nodes))),
          list(track.height = circlize::mm_h(0.1))
        )
      )
      circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
        circlize::circos.text(
          circlize::CELL_META$xcenter,
          circlize::CELL_META$ylim[1] + 0.15,
          circlize::CELL_META$sector.index,
          facing = "clockwise",
          niceFacing = TRUE,
          adj = c(0, 0.5)
        )
      }, bg.border = NA)
      circlize::circos.track(track.index = 2, panel.fun = function(x, y) {
        for (si in circlize::get.all.sector.index()) {
          start.degree <- circlize::get.cell.meta.data("cell.start.degree", sector.index = si)
          end.degree <- circlize::get.cell.meta.data("cell.end.degree", sector.index = si)
          if (abs(end.degree - start.degree) > 2) {
            circlize::circos.axis(
              h = "top",
              labels.cex = 0.7,
              labels.niceFacing = TRUE,
              sector.index = si
            )
          }
        }
      }, bg.border = NA)
    }
  }

  # Wrap the circlize plot as patchwork element
  p <- patchwork::wrap_elements(full = p)

  # Add title/subtitle annotations if provided
  if (!is.null(title) || !is.null(subtitle)) {
    p <- p + patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = do.call(theme, theme_args)
    )
  }

  # Attach data to plot object for external access
  p$data <- data

  # Calculate plot dimensions based on label length for rotated labels
  base_size <- 7
  if (isTRUE(labels_rot)) {
    maxchar <- max(c(nchar(froms), nchar(tos)))
    if (maxchar < 16) {
      base_size <- base_size + 2
    } else if (maxchar < 32) {
      base_size <- base_size + 4
    } else {
      base_size <- base_size + 6
    }
  }

  # Set height/width attributes for patchwork layout
  attr(p, "height") <- base_size
  attr(p, "width") <- base_size

  return(p)
}

#' Chord / Circos Plot
#'
#' @description
#' Creates a chord diagram to visualize relationships between two categorical variables
#' using the circlize package. Chord diagrams are useful for displaying flows or
#' connections between entities, with link width proportional to relationship strength.
#'
#' `CircosPlot` is an alias of `ChordPlot`.
#'
#' @inheritParams parameters
#' @param y Column name for relationship values. If NULL, counts relationships
#' @param from Column name(s) for source nodes. Multiple columns will be concatenated
#' @param from_sep Separator for concatenating multiple from columns (default: "_")
#' @param to Column name(s) for target nodes. Multiple columns will be concatenated
#' @param to_sep Separator for concatenating multiple to columns (default: "_")
#' @param flip Logical. Whether to flip source and target nodes (default: FALSE)
#' @param links_color Character. Color links by source ("from") or target ("to") nodes
#' @param labels_rot Logical. Whether to rotate labels by 90 degrees (default: FALSE)
#' @param ... Additional arguments passed to circlize functions
#'
#' @return
#' If \code{combine = TRUE}, returns a combined patchwork object.
#' If \code{combine = FALSE}, returns a list of individual chord diagrams.
#'
#' @rdname chordplot
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'   nodes1 = sample(c("Source1", "Source2", "Source3"), 10, replace = TRUE),
#'   nodes2 = sample(letters[1:3], 10, replace = TRUE),
#'   y = sample(1:5, 10, replace = TRUE)
#' )
#'
#' # Basic chord plot
#' ChordPlot(data, from = "nodes1", to = "nodes2")
#'
#' # With rotated labels and colored by target
#' ChordPlot(data,
#'   from = "nodes1", to = "nodes2",
#'   links_color = "to", labels_rot = TRUE
#' )
#'
#' # With values
#' ChordPlot(data, from = "nodes1", to = "nodes2", y = "y")
#'
#' # Split by variable
#' ChordPlot(data, from = "nodes1", to = "nodes2", split_by = "y")
#'
#' # With custom palettes per split
#' ChordPlot(data,
#'   from = "nodes1", to = "nodes2", split_by = "y",
#'   palette = c("1" = "Reds", "2" = "Blues", "3" = "Greens")
#' )
#'
#' # Flip source and target
#' ChordPlot(data, from = "nodes1", to = "nodes2", flip = TRUE)
#' }
ChordPlot <- function(
    data,
    y = NULL,
    from = NULL,
    from_sep = "_",
    to = NULL,
    to_sep = "_",
    split_by = NULL,
    split_by_sep = "_",
    flip = FALSE,
    links_color = c("from", "to"),
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 0.5,
    labels_rot = FALSE,
    title = NULL,
    subtitle = NULL,
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
  # Validate common arguments
  validate_common_args(
    seed = seed,
    split_by = split_by,
    theme = theme,
    palette = palette,
    alpha = alpha
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

  # Split data if requested, preserving factor level order
  if (!is.null(split_by)) {
    datas <- split(data, data[[split_by]])
    datas <- datas[levels(data[[split_by]])]
  } else {
    datas <- list(data)
    names(datas) <- "..."
  }

  # Normalize palette and palcolor for each split level
  palette <- check_palette(palette, names(datas))
  palcolor <- check_palcolor(palcolor, names(datas))

  # Build individual chord plots for each split level
  plots <- lapply(
    names(datas), function(nm) {
      # Generate default title: use split level name unless single plot
      default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm

      # Allow title to be a function for dynamic generation
      if (is.function(title)) {
        plot_title <- title(default_title)
      } else {
        plot_title <- title %||% default_title
      }

      ChordPlotAtomic(
        datas[[nm]],
        y = y,
        from = from,
        from_sep = from_sep,
        to = to,
        to_sep = to_sep,
        flip = flip,
        links_color = links_color,
        theme = theme,
        theme_args = theme_args,
        palette = palette[[nm]],
        palcolor = palcolor[[nm]],
        alpha = alpha,
        labels_rot = labels_rot,
        title = plot_title,
        subtitle = subtitle,
        ...
      )
    }
  )

  # Combine plots
  combine_plots(
    plots,
    combine = combine,
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    axes = axes,
    axis_titles = axis_titles,
    guides = guides,
    design = design
  )
}

#' @export
#' @rdname chordplot
CircosPlot <- ChordPlot
