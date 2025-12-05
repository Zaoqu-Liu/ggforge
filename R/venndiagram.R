#' Venn Diagram
#'
#' @description
#' Create Venn diagrams to visualize overlaps between sets.
#' Supports multiple input formats (long, wide, list) and various styling options.
#'
#' @inheritParams parameters
#' @param in_form Format of input data. One of:
#'   - "auto": Automatically detect format (default)
#'   - "long": Long format with group_by and id_by columns
#'   - "wide": Wide format with logical/0-1 columns
#'   - "list": Named list of character vectors
#'   - "venn": Pre-processed VennPlotData object
#' @param group_by Column(s) for grouping when in_form is "long" or "wide"
#' @param group_by_sep Separator for concatenating multiple group_by columns
#' @param id_by Column containing IDs when in_form is "long"
#' @param label Label style: "count", "percent", "both", "none", or a custom function
#' @param label_fg Color of label text
#' @param label_size Size of label text (default scaled by base_size)
#' @param label_bg Background color of labels
#' @param label_bg_r Radius of label background
#' @param fill_mode Fill coloring mode:
#'   - "count": Color by intersection count (continuous)
#'   - "set": Color by set membership (discrete, blended)
#'   - "count_rev": Reverse of count mode
#' @param fill_name Name for the fill legend
#'
#' @return A ggplot object or list/combined plots
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- list(
#'   A = sort(sample(letters, 8)),
#'   B = sort(sample(letters, 8)),
#'   C = sort(sample(letters, 8)),
#'   D = sort(sample(letters, 8))
#' )
#'
#' VennDiagram(data)
#' VennDiagram(data, fill_mode = "set")
#' VennDiagram(data, label = "both")
#' VennDiagram(data, palette = "material-indigo", alpha = 0.6)
#' }
VennDiagram <- function(
    data,
    in_form = c("auto", "long", "wide", "list", "venn"),
    split_by = NULL,
    split_by_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    id_by = NULL,
    label = "count",
    label_fg = "black",
    label_size = NULL,
    label_bg = "white",
    label_bg_r = 0.1,
    fill_mode = "count",
    fill_name = NULL,
    palette = ifelse(fill_mode == "set", "Paired", "Spectral"),
    palcolor = NULL,
    alpha = 1,
    theme = "theme_ggforge",
    theme_args = list(),
    title = NULL,
    subtitle = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    seed = 8525,
    axes = NULL,
    axis_titles = NULL,
    guides = NULL,
    design = NULL,
    ...) {
  # Validate common arguments
  validate_common_args(
    seed = seed,
    theme = theme,
    palette = palette,
    alpha = alpha,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Match argument
  in_form <- match.arg(in_form)

  # Process theme
  theme <- process_theme(theme)

  # Check split_by only works with data frames
  if (!is.null(split_by) && !inherits(data, "data.frame")) {
    stop("'split_by' is only available for data frames", call. = FALSE)
  }

  # Validate split_by column
  split_by <- validate_columns(
    data, split_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = split_by_sep
  )

  # Split data if needed
  if (!is.null(split_by)) {
    datas <- split(data, data[[split_by]])
    # Keep the order of levels
    datas <- datas[levels(data[[split_by]])]
  } else {
    datas <- list("..." = data)
  }

  # Check palette, palcolor, legend settings
  palette <- check_palette(palette, names(datas))
  palcolor <- check_palcolor(palcolor, names(datas))
  legend.direction <- check_legend_param(legend.direction, names(datas), "legend.direction")
  legend.position <- check_legend_param(legend.position, names(datas), "legend.position")

  # Create plots
  plots <- lapply(
    names(datas), function(nm) {
      default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
      if (is.function(title)) {
        title_use <- title(default_title)
      } else {
        title_use <- title %||% default_title
      }

      # Add split indicator if needed
      if (length(datas) > 1 || !identical(nm, "...")) {
        datas[[nm]]$.split <- nm
      }

      VennDiagramAtomic(
        data = datas[[nm]],
        in_form = in_form,
        group_by = group_by,
        group_by_sep = group_by_sep,
        id_by = id_by,
        label = label,
        label_fg = label_fg,
        label_size = label_size,
        label_bg = label_bg,
        label_bg_r = label_bg_r,
        fill_mode = fill_mode,
        fill_name = fill_name,
        palette = palette[[nm]],
        palcolor = palcolor[[nm]],
        alpha = alpha,
        theme = theme,
        theme_args = theme_args,
        title = title_use,
        subtitle = subtitle,
        legend.position = legend.position[[nm]],
        legend.direction = legend.direction[[nm]],
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


#' Venn Diagram Atomic
#'
#' @description
#' Creates a single Venn diagram without splitting
#'
#' @inheritParams parameters
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_polygon geom_path coord_equal scale_x_continuous
#' @importFrom ggplot2 scale_fill_gradientn guide_colorbar labs theme element_blank expansion
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang sym "%||%"
#' @importFrom scales percent
VennDiagramAtomic <- function(
    data,
    in_form = "auto",
    group_by = NULL,
    group_by_sep = "_",
    id_by = NULL,
    label = "count",
    label_fg = "black",
    label_size = NULL,
    label_bg = "white",
    label_bg_r = 0.1,
    fill_mode = "count",
    fill_name = NULL,
    palette = "Spectral",
    palcolor = NULL,
    alpha = 1,
    theme = "theme_ggforge",
    theme_args = list(),
    title = NULL,
    subtitle = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    ...) {
  # Get ggplot function (with gglogger support)
  ggplot <- get_ggplot()

  # Get base size for scaling text elements
  base_size <- theme_args$base_size %||% 12
  text_size_scale <- base_size / 12

  # Extract and remove split indicator from data
  s <- data$.split
  data$.split <- NULL

  # Validate fill_mode argument
  fill_mode <- match.arg(fill_mode, c("count", "set", "count_rev"))

  # Prepare Venn data from various input formats
  data <- prepare_venn_data(data, in_form, group_by, group_by_sep, id_by)

  # Extract Venn diagram components using ggVennDiagram
  data_regionedge <- ggVennDiagram::venn_regionedge(data)
  data_setedge <- ggVennDiagram::venn_setedge(data)
  data_regionlabel <- ggVennDiagram::venn_regionlabel(data)
  data_regionlabel$.split <- if (is.null(s)) NULL else s[1]
  data_setlabel <- ggVennDiagram::venn_setlabel(data)

  # Calculate fill colors for regions when fill_mode is "set"
  if (fill_mode == "set") {
    # Get base colors for each set
    colors <- get_palette(
      data$setLabel$id,
      palette = palette,
      palcolor = palcolor
    )

    ids <- unique(data_regionedge$id)

    # Blend colors for intersections
    blended_colors <- sapply(ids, function(id) {
      parts <- strsplit(id, "/")[[1]]
      blend_colors(colors[parts])
    })
    names(blended_colors) <- ids

    data_regionedge$fill <- blended_colors[data_regionedge$id]
    data_setedge$color <- colors[data_setedge$id]
  }

  # Prepare set labels (name + count)
  data_setlabel$label <- paste0(data_setlabel$name, "\n(", data_setlabel$count, ")")

  # Format region labels based on label type
  if (identical(label, "percent")) {
    data_regionlabel$label <- scales::percent(
      data_regionlabel$count / sum(data_regionlabel$count)
    )
  } else if (identical(label, "both")) {
    data_regionlabel$label <- paste(
      data_regionlabel$count, "\n",
      scales::percent(data_regionlabel$count / sum(data_regionlabel$count))
    )
  } else if (identical(label, "count")) {
    data_regionlabel$label <- data_regionlabel$count
  } else if (is.function(label)) {
    # Allow custom label formatting function
    data_regionlabel$label <- label(data_regionlabel)
  } else if (!identical(label, "none")) {
    stop("Parameter 'label' must be 'count', 'percent', 'both', 'none', or a function", call. = FALSE)
  }

  # Initialize plot
  p <- ggplot()

  # Add regions and edges based on fill_mode
  if (fill_mode == "set") {
    # Discrete fill with blended colors for intersections
    p <- p +
      ggplot2::geom_polygon(
        data = data_regionedge,
        ggplot2::aes(!!rlang::sym("X"), !!rlang::sym("Y"), group = !!rlang::sym("id")),
        fill = data_regionedge$fill,
        alpha = alpha
      ) +
      ggplot2::geom_path(
        data = data_setedge,
        ggplot2::aes(!!rlang::sym("X"), !!rlang::sym("Y"), group = !!rlang::sym("id")),
        color = data_setedge$color,
        show.legend = FALSE
      )
  } else {
    # Continuous fill by intersection count
    p <- p +
      ggplot2::geom_polygon(
        data = data_regionedge,
        ggplot2::aes(!!rlang::sym("X"), !!rlang::sym("Y"), fill = !!rlang::sym("count"), group = !!rlang::sym("id")),
        alpha = alpha
      ) +
      ggplot2::scale_fill_gradientn(
        n.breaks = 3,
        colors = get_palette(
          n = 100,
          palette = palette,
          palcolor = palcolor,
          reverse = grepl("rev", fill_mode),
          keep_names = FALSE
        ),
        na.value = "grey80",
        guide = ggplot2::guide_colorbar(
          title = fill_name %||% "",
          frame.colour = "black",
          ticks.colour = "black",
          frame.linewidth = 0.3,
          ticks.linewidth = 0.3,
          title.hjust = 0
        )
      ) +
      ggplot2::geom_path(
        data = data_setedge,
        ggplot2::aes(!!rlang::sym("X"), !!rlang::sym("Y"), group = !!rlang::sym("id")),
        color = "grey20",
        show.legend = FALSE
      )
  }

  # Add region labels if requested
  if (!identical(label, "none")) {
    p <- p + ggrepel::geom_text_repel(
      data = data_regionlabel,
      ggplot2::aes(!!rlang::sym("X"), !!rlang::sym("Y"), label = !!rlang::sym("label")),
      color = label_fg,
      bg.color = label_bg,
      bg.r = label_bg_r,
      size = label_size %||% text_size_scale * 3.5,
      point.size = NA,
      max.overlaps = 100,
      force = 0,
      min.segment.length = 0,
      segment.colour = "black"
    )
  }

  # Add set labels with names and counts
  p <- p + ggrepel::geom_text_repel(
    data = data_setlabel,
    ggplot2::aes(!!rlang::sym("X"), !!rlang::sym("Y"), label = !!rlang::sym("label")),
    color = label_fg,
    bg.color = label_bg,
    bg.r = label_bg_r,
    size = label_size %||% text_size_scale * 4,
    fontface = "bold",
    point.size = NA,
    max.overlaps = 100,
    force = 0,
    min.segment.length = 0,
    segment.colour = "black"
  )

  # Add labels and apply theme
  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::coord_equal() +
    do.call(theme, theme_args) +
    ggplot2::theme(
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

  # Expand x-axis to accommodate long set names
  maxchars <- max(sapply(data_setlabel$name, nchar))
  p <- p + ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(add = 0.001 * maxchars * text_size_scale, mult = 0.1)
  )

  # Calculate optimal plot dimensions based on legend configuration
  height <- 5.5
  width <- 6
  if (fill_mode != "set") {
    if (legend.position %in% c("right", "left")) {
      width <- width + 1
    } else if (legend.direction == "horizontal") {
      height <- height + 1
    } else {
      width <- width + 2
    }
  }

  # Store dimensions as attributes for patchwork
  attr(p, "height") <- height
  attr(p, "width") <- width

  return(p)
}


#' Detect Venn data type
#'
#' @description
#' Auto-detect the format of input data for Venn diagram based on structure
#'
#' @param data A data frame, list, or VennPlotData object
#' @param group_by Column(s) for grouping
#' @param id_by Column for IDs
#' @return Character string: "long", "wide", "list", or "venn"
#' @keywords internal
detect_venn_datatype <- function(data, group_by = NULL, id_by = NULL) {
  if (inherits(data, "data.frame")) {
    # Single group_by column indicates long format (with id_by)
    if (length(group_by) < 2 && !is.null(group_by)) {
      return("long")
    } else {
      # Multiple or no group_by indicates wide format (logical columns)
      return("wide")
    }
  } else if (inherits(data, "VennPlotData")) {
    return("venn")
  } else if (is.list(data)) {
    return("list")
  }

  stop("Parameter 'data' must be a data frame, list, or VennPlotData object", call. = FALSE)
}


#' Prepare Venn data
#'
#' @description
#' Prepare data for Venn diagram plotting. Converts various input formats
#' to VennPlotData object used by ggVennDiagram.
#'
#' Input formats:
#' - Long format: group_by and id_by columns
#'   \preformatted{
#'   group_by id_by
#'   A        a1
#'   A        a2
#'   B        a1
#'   B        a3
#'   }
#' - Wide format: Logical or 0/1 columns
#'   \preformatted{
#'   A     B
#'   TRUE  TRUE
#'   TRUE  FALSE
#'   FALSE TRUE
#'   }
#' - List format: Named list of character vectors
#'   \preformatted{
#'   list(A = c("a1", "a2"), B = c("a1", "a3"))
#'   }
#'
#' @param data Input data
#' @param in_form Data format ("auto", "long", "wide", "list", or "venn")
#' @param group_by Column(s) for grouping
#' @param group_by_sep Separator for concatenating multiple columns
#' @param id_by Column containing IDs
#' @return VennPlotData object
#' @keywords internal
prepare_venn_data <- function(data, in_form = "auto", group_by = NULL, group_by_sep = "_", id_by = NULL) {
  # Auto-detect format
  if (in_form == "auto") {
    in_form <- detect_venn_datatype(data, group_by, id_by)
  }

  # Return pre-processed VennPlotData as-is
  if (in_form == "venn") {
    if (!is.null(group_by)) {
      warning("'group_by' argument is ignored when data is already a VennPlotData object", immediate. = TRUE, call. = FALSE)
    }
    return(data)
  }

  # Convert input data to list format
  if (in_form == "list") {
    if (!is.list(data)) {
      stop("Parameter 'data' must be a list when in_form = 'list'", call. = FALSE)
    }
    listdata <- data
  } else if (in_form == "long") {
    # Long format: split IDs by group_by column
    group_by <- validate_columns(
      data, group_by,
      force_factor = TRUE,
      allow_multi = TRUE,
      concat_multi = TRUE,
      concat_sep = group_by_sep
    )
    listdata <- split(data[[id_by]], data[[group_by]])
  } else {
    # Wide format: extract IDs where logical/0-1 columns are TRUE
    group_by <- validate_columns(data, group_by, allow_multi = TRUE)
    if (is.null(group_by)) {
      group_by <- colnames(data)
    }

    # Validate that columns contain only logical or 0/1 values
    for (g in group_by) {
      if (!is.logical(data[[g]]) && !all(data[[g]] %in% c(0, 1))) {
        stop(
          sprintf("Column '%s' must be logical or contain only 0/1 values when in_form = 'wide'", g),
          call. = FALSE
        )
      }
    }

    # Generate unique IDs and extract sets
    data$.id <- paste0("id", seq_len(nrow(data)))
    listdata <- lapply(group_by, function(g) {
      data[as.logical(data[[g]]), ".id", drop = TRUE]
    })
    names(listdata) <- group_by

    # Remove empty groups and warn user
    for (nm in names(listdata)) {
      if (length(listdata[[nm]]) == 0) {
        warning(sprintf("Group '%s' contains no elements and will be ignored", nm), immediate. = TRUE, call. = FALSE)
        listdata[[nm]] <- NULL
      }
    }
  }

  # Convert to VennPlotData using ggVennDiagram
  ggVennDiagram::process_data(ggVennDiagram::Venn(listdata))
}
