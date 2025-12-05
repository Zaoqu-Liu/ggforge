#' Network Plot
#'
#' @description
#' Visualize network graphs with nodes and links using igraph layouts and ggraph.
#' Supports directed and undirected graphs, customizable node and link aesthetics,
#' clustering, and various layout algorithms.
#'
#' @inheritParams parameters
#' @param links A data frame containing the links between nodes.
#' @param nodes A data frame containing the nodes (optional).
#'   Node names are extracted from the links data frame if not provided.
#'   If `"@nodes"` is provided, the nodes data frame will be extracted from
#'   the `nodes` attribute of the links data frame.
#' @param from Column name(s) for source nodes. Default is the first column.
#' @param from_sep Separator for concatenating multiple `from` columns.
#' @param to Column name(s) for target nodes. Default is the second column.
#' @param to_sep Separator for concatenating multiple `to` columns.
#' @param node_by Column name(s) for node identifiers. Default is the first column.
#' @param node_by_sep Separator for concatenating multiple `node_by` columns.
#' @param link_weight_by Numeric value or column name for link width.
#' @param link_weight_name Legend title for link weight.
#' @param link_type_by Link line type: "solid", "dashed", "dotted", or a column name.
#' @param link_type_name Legend title for link type.
#' @param node_size_by Numeric value or column name for node size.
#' @param node_size_name Legend title for node size.
#' @param node_color_by Color value or column name for node color (border).
#' @param node_color_name Legend title for node color.
#' @param node_shape_by Numeric value or column name for node shape.
#' @param node_shape_name Legend title for node shape.
#' @param node_fill_by Fill color value or column name for node fill.
#' @param node_fill_name Legend title for node fill.
#' @param link_alpha Transparency for links (0-1).
#' @param node_alpha Transparency for node fill (0-1).
#' @param node_stroke Width of node borders.
#' @param cluster_scale Which aesthetic to use for clusters: "fill", "color", or "shape".
#' @param node_size_range Range for node sizes.
#' @param link_weight_range Range for link widths.
#' @param link_arrow_offset Offset for link arrows to avoid overlapping nodes.
#' @param link_curvature Curvature of links (0 = straight).
#' @param link_color_by Link coloring: "from", "to", or a column name.
#' @param link_color_name Legend title for link color.
#' @param link_palette Palette for link colors.
#' @param link_palcolor Custom colors for link palette.
#' @param directed Whether the graph is directed.
#' @param layout Layout algorithm: "circle", "tree", "grid", or an igraph layout name.
#' @param cluster Clustering method: "none", "fast_greedy", "walktrap",
#'   "edge_betweenness", "infomap", or an igraph clustering function.
#' @param add_mark Whether to add visual marks around clusters.
#' @param mark_expand Expansion of cluster marks.
#' @param mark_type Type of cluster marks: "hull", "ellipse", "rect", or "circle".
#' @param mark_alpha Transparency of cluster marks.
#' @param mark_linetype Line type of cluster marks.
#' @param add_label Whether to add labels to nodes.
#' @param label_size Size of node labels.
#' @param label_fg Foreground color of labels.
#' @param label_bg Background color of labels.
#' @param label_bg_r Background ratio for labels.
#' @param arrow Arrow specification for directed graphs.
#' @param split_nodes Whether to split nodes data when splitting by a column.
#'
#' @return A ggplot object, wrap_plots object, or a list of ggplot objects
#' @export
#' @examples
#' \dontrun{
#' actors <- data.frame(
#'   name = c("Alice", "Bob", "Cecil", "David", "Esmeralda"),
#'   age = c(48, 33, 45, 34, 21),
#'   shape = c(21, 22, 21, 22, 23),
#'   gender = c("F", "M", "F", "M", "F")
#' )
#' relations <- data.frame(
#'   from = c(
#'     "Bob", "Cecil", "Cecil", "David", "David", "Esmeralda", "Bob", "Alice",
#'     "Cecil", "David"
#'   ),
#'   to = c(
#'     "Alice", "Bob", "Alice", "Alice", "Bob", "Alice", "Bob", "Alice", "Cecil",
#'     "David"
#'   ),
#'   friendship = c(4, 5, 5, 2, 1, 1, 2, 1, 3, 4),
#'   type = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
#' )
#' Network(relations, actors)
#' Network(relations, actors,
#'   link_weight_by = "friendship", node_size_by = "age",
#'   node_fill_by = "gender", link_color_by = "to",
#'   layout = "circle", link_curvature = 0.2
#' )
#' Network(relations, actors,
#'   layout = "tree", directed = FALSE, cluster = "fast_greedy",
#'   add_mark = TRUE
#' )
#' }
Network <- function(
    links, nodes = NULL,
    split_by = NULL,
    split_by_sep = "_",
    split_nodes = FALSE,
    from = NULL,
    from_sep = "_",
    to = NULL,
    to_sep = "_",
    node_by = NULL,
    node_by_sep = "_",
    link_weight_by = 2,
    link_weight_name = NULL,
    link_type_by = "solid",
    link_type_name = NULL,
    node_size_by = 15,
    node_size_name = NULL,
    node_color_by = "black",
    node_color_name = NULL,
    node_shape_by = 21,
    node_shape_name = NULL,
    node_fill_by = "grey20",
    node_fill_name = NULL,
    link_alpha = 1,
    node_alpha = 0.95,
    node_stroke = 1.5,
    cluster_scale = c("fill", "color", "shape"),
    node_size_range = c(5, 20),
    link_weight_range = c(0.5, 5),
    link_arrow_offset = 20,
    link_curvature = 0,
    link_color_by = "from",
    link_color_name = NULL,
    palette = "Paired",
    palcolor = NULL,
    link_palette = ifelse(link_color_by %in% c("from", "to"), palette, "Set1"),
    link_palcolor = if (link_color_by %in% c("from", "to")) palcolor else NULL,
    directed = TRUE,
    layout = "circle",
    cluster = "none",
    add_mark = FALSE,
    mark_expand = ggplot2::unit(10, "mm"),
    mark_type = c("hull", "ellipse", "rect", "circle"),
    mark_alpha = 0.1,
    mark_linetype = 1,
    add_label = TRUE,
    label_size = 3,
    label_fg = "white",
    label_bg = "black",
    label_bg_r = 0.1,
    arrow = ggplot2::arrow(type = "closed", length = ggplot2::unit(0.1, "inches")),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    aspect.ratio = 1,
    theme = "theme_ggforge",
    theme_args = list(),
    legend.position = "right",
    legend.direction = "vertical",
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
    alpha = link_alpha,
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Process theme
  theme <- process_theme(theme)

  # Handle split_nodes: disable if no split_by
  if (is.null(split_by)) {
    split_nodes <- FALSE
  }

  # Validate split_by on links
  l_split_by <- validate_columns(
    links, split_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = split_by_sep
  )

  # Handle split_nodes: validate on nodes if needed
  if (isTRUE(split_nodes) && !is.null(nodes)) {
    # Extract nodes from attribute if needed
    if (is.character(nodes) && length(nodes) == 1 && startsWith(nodes, "@")) {
      nodes <- attr(links, substring(nodes, 2))
    }

    n_split_by <- validate_columns(
      nodes, split_by,
      force_factor = TRUE,
      allow_multi = TRUE,
      concat_multi = TRUE,
      concat_sep = split_by_sep
    )

    if (!identical(l_split_by, n_split_by)) {
      stop("The `split_by` columns in `links` and `nodes` must be the same.", call. = FALSE)
    }
  }

  # Split data if needed
  if (!is.null(split_by)) {
    linkss <- split(links, links[[l_split_by]])

    if (isTRUE(split_nodes) && !is.null(nodes)) {
      nodess <- split(nodes, nodes[[n_split_by]])
      nms <- names(nodess)
      linkss <- lapply(nms, function(nm) {
        dat <- linkss[[nm]]
        attr(dat, "nodes") <- nodess[[nm]]
        dat
      })
      names(linkss) <- nms
      nodes <- "@nodes" # Signal to atomic function to extract from attribute
    } else {
      linkss <- linkss[levels(links[[l_split_by]])]
    }
  } else {
    linkss <- list(links)
    names(linkss) <- "..."
  }

  # Normalize palette and other split-dependent parameters
  split_names <- names(linkss)
  palette <- check_palette(palette, split_names)
  palcolor <- check_palcolor(palcolor, split_names)
  link_palette <- check_palette(link_palette, split_names)
  link_palcolor <- check_palcolor(link_palcolor, split_names)
  legend.position <- check_legend_param(legend.position, split_names, "legend.position")
  legend.direction <- check_legend_param(legend.direction, split_names, "legend.direction")

  # Build plots for each split
  plots <- lapply(split_names, function(nm) {
    default_title <- if (length(linkss) == 1 && identical(nm, "...")) NULL else nm
    plot_title <- if (is.function(title)) {
      title(default_title)
    } else {
      title %||% default_title
    }

    NetworkAtomic(
      links = linkss[[nm]],
      nodes = nodes,
      from = from,
      from_sep = from_sep,
      to = to,
      to_sep = to_sep,
      node_by = node_by,
      node_by_sep = node_by_sep,
      link_weight_by = link_weight_by,
      link_weight_name = link_weight_name,
      link_type_by = link_type_by,
      link_type_name = link_type_name,
      node_size_by = node_size_by,
      node_size_name = node_size_name,
      node_color_by = node_color_by,
      node_color_name = node_color_name,
      node_shape_by = node_shape_by,
      node_shape_name = node_shape_name,
      node_fill_by = node_fill_by,
      node_fill_name = node_fill_name,
      link_alpha = link_alpha,
      node_alpha = node_alpha,
      node_stroke = node_stroke,
      cluster_scale = cluster_scale,
      node_size_range = node_size_range,
      link_weight_range = link_weight_range,
      link_arrow_offset = link_arrow_offset,
      link_curvature = link_curvature,
      link_color_by = link_color_by,
      link_color_name = link_color_name,
      palette = palette[[nm]],
      palcolor = palcolor[[nm]],
      link_palette = link_palette[[nm]],
      link_palcolor = link_palcolor[[nm]],
      directed = directed,
      layout = layout,
      cluster = cluster,
      add_mark = add_mark,
      mark_expand = mark_expand,
      mark_type = mark_type,
      mark_alpha = mark_alpha,
      mark_linetype = mark_linetype,
      add_label = add_label,
      label_size = label_size,
      label_fg = label_fg,
      label_bg = label_bg,
      label_bg_r = label_bg_r,
      arrow = arrow,
      title = plot_title,
      subtitle = subtitle,
      xlab = xlab,
      ylab = ylab,
      aspect.ratio = aspect.ratio,
      theme = theme,
      theme_args = theme_args,
      legend.position = legend.position[[nm]],
      legend.direction = legend.direction[[nm]],
      ...
    )
  })

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

#' Network Plot Atomic
#'
#' @description
#' Creates a single network plot without splitting the data.
#'
#' @inheritParams Network
#' @keywords internal
#' @importFrom utils getFromNamespace
#' @importFrom dplyr rename relocate
#' @importFrom ggplot2 aes geom_point scale_x_continuous scale_y_continuous labs theme
#' @importFrom ggplot2 scale_size_continuous scale_linetype_discrete guide_legend unit
#' @importFrom ggplot2 scale_color_manual scale_fill_manual element_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggnewscale new_scale_color new_scale_fill
#' @importFrom ggraph ggraph geom_edge_arc geom_edge_loop scale_edge_width_continuous
#' @importFrom ggraph scale_edge_linetype_discrete scale_edge_color_gradientn
#' @importFrom ggraph scale_edge_color_manual circle
#' @importFrom rlang sym "%||%"
NetworkAtomic <- function(
    links,
    nodes = NULL,
    from = NULL,
    from_sep = "_",
    to = NULL,
    to_sep = "_",
    node_by = NULL,
    node_by_sep = "_",
    link_weight_by = 2,
    link_weight_name = NULL,
    link_type_by = "solid",
    link_type_name = NULL,
    node_size_by = 15,
    node_size_name = NULL,
    node_color_by = "black",
    node_color_name = NULL,
    node_shape_by = 21,
    node_shape_name = NULL,
    node_fill_by = "grey20",
    node_fill_name = NULL,
    link_alpha = 1,
    node_alpha = 0.95,
    node_stroke = 1.5,
    cluster_scale = c("fill", "color", "shape"),
    node_size_range = c(5, 20),
    link_weight_range = c(0.5, 5),
    link_arrow_offset = 20,
    link_curvature = 0,
    link_color_by = "from",
    link_color_name = NULL,
    palette = "Paired",
    palcolor = NULL,
    link_palette = "Set1",
    link_palcolor = NULL,
    directed = TRUE,
    layout = "circle",
    cluster = "none",
    add_mark = FALSE,
    mark_expand = ggplot2::unit(10, "mm"),
    mark_type = c("hull", "ellipse", "rect", "circle"),
    mark_alpha = 0.1,
    mark_linetype = 1,
    add_label = TRUE,
    label_size = 3,
    label_fg = "white",
    label_bg = "black",
    label_bg_r = 0.1,
    arrow = ggplot2::arrow(type = "closed", length = ggplot2::unit(0.1, "inches")),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    aspect.ratio = 1,
    theme = "theme_ggforge",
    theme_args = list(),
    legend.position = "right",
    legend.direction = "vertical",
    ...) {
  # Match cluster_scale argument
  cluster_scale <- match.arg(cluster_scale)

  # Calculate text size scale
  base_size <- theme_args$base_size %||% 12
  text_size_scale <- base_size / 12

  # Extract nodes from attribute if needed
  if (is.character(nodes) && length(nodes) == 1 && startsWith(nodes, "@")) {
    nodes <- attr(links, substring(nodes, 2))
  }

  # Set default column names for links
  if (is.null(from)) {
    from <- "from"
    if (!from %in% colnames(links)) {
      colnames(links)[1] <- from
    }
  }
  if (is.null(to)) {
    to <- "to"
    if (!to %in% colnames(links)) {
      colnames(links)[2] <- to
    }
  }

  # Set default column name for nodes
  if (is.null(node_by) && !is.null(nodes)) {
    node_by <- "name"
    if (!node_by %in% colnames(nodes)) {
      colnames(nodes)[1] <- node_by
    }
  }

  # Validate and process link columns
  from <- validate_columns(
    links, from,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = from_sep
  )
  to <- validate_columns(
    links, to,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = to_sep
  )

  # Rename and relocate link columns for igraph
  links <- links %>%
    dplyr::rename(from = !!sym(from), to = !!sym(to)) %>%
    dplyr::relocate(!!sym("from"), !!sym("to"))

  # Validate and process node columns
  if (!is.null(nodes)) {
    node_by <- validate_columns(
      nodes, node_by,
      force_factor = TRUE,
      allow_multi = TRUE,
      concat_multi = TRUE,
      concat_sep = node_by_sep
    )
    nodes <- nodes %>%
      dplyr::rename(name = !!sym(node_by)) %>%
      dplyr::relocate(!!sym("name"))
  }

  # Create igraph object
  graph <- igraph::graph_from_data_frame(d = links, vertices = nodes, directed = directed)

  # Process layout
  if (inherits(layout, "igraph_layout_spec")) {
    layout <- igraph::layout_(graph, layout)
  } else {
    if (layout %in% c("circle", "tree", "grid")) {
      layout <- switch(layout,
        "circle" = igraph::layout_in_circle(graph),
        "tree" = igraph::layout_as_tree(graph),
        "grid" = igraph::layout_on_grid(graph)
      )
    } else {
      lofun <- getFromNamespace(paste0("layout_with_", layout), "igraph")
      layout <- lofun(graph)
    }
  }

  # Extract graph data
  df <- igraph::as_data_frame(graph, what = "both")
  df_nodes <- df$vertices
  df_nodes$x <- layout[, 1]
  df_nodes$y <- layout[, 2]
  df_edges <- df$edges
  rm(df)

  # Build node layer arguments
  node_layer_args <- list(
    mapping = list(aes(x = !!sym("x"), y = !!sym("y"))),
    stroke = node_stroke
  )

  # Handle node size
  if (is.numeric(node_size_by)) {
    node_layer_args$size <- node_size_by
    node_size_by_guide <- "none"
  } else {
    node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(size = !!sym(node_size_by))
    node_size_by_guide <- "guide"
  }

  # Handle node color
  if (!node_color_by %in% colnames(df_nodes)) {
    node_layer_args$color <- node_color_by
    node_color_by_guide <- "none"
  } else {
    node_color_by <- validate_columns(df_nodes, node_color_by, force_factor = TRUE)
    node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(color = !!sym(node_color_by))
    node_color_by_guide <- "guide"
  }

  # Handle node shape
  if (is.numeric(node_shape_by)) {
    node_layer_args$shape <- node_shape_by
    node_shape_by_guide <- "none"
  } else {
    node_shape_by <- validate_columns(df_nodes, node_shape_by, force_factor = TRUE)
    node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(shape = as.factor(!!sym(node_shape_by)))
    node_shape_by_guide <- "guide"
  }

  # Handle node fill
  if (!node_fill_by %in% colnames(df_nodes)) {
    node_layer_args$fill <- node_fill_by
    node_fill_by_guide <- "none"
  } else {
    node_fill_by <- validate_columns(df_nodes, node_fill_by, force_factor = TRUE)
    node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(fill = !!sym(node_fill_by))
    node_fill_by_guide <- "guide"
  }

  # Build link layer arguments
  link_layer_args <- list(alpha = link_alpha, check_overlap = TRUE, mapping = list())
  if (isTRUE(directed)) {
    link_layer_args$arrow <- arrow
  }

  # Handle link color
  if (link_color_by %in% colnames(df_edges) && !link_color_by %in% c("from", "to")) {
    link_color_by_guide <- "guide"
    link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(color = !!sym(link_color_by))
  } else {
    link_color_by_guide <- "none"
    if (node_shape_by %in% 21:25) {
      # Use node fill colors for links
      if (node_fill_by_guide == "none") {
        link_layer_args$color <- node_fill_by
      } else {
        link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(color = I(!!sym(paste0("node.", node_fill_by))))
        # Map node_fill_by from nodes data to links data
        df_edges[[paste0("node.", node_fill_by)]] <- get_palette(
          levels(df_nodes[[node_fill_by]]),
          palette = link_palette, palcolor = link_palcolor
        )[df_nodes[df_edges[[link_color_by]], node_fill_by]]
        graph <- igraph::graph_from_data_frame(d = df_edges, vertices = df_nodes, directed = directed)
      }
    } else {
      # Use node color for links
      if (node_color_by_guide == "none") {
        link_layer_args$color <- node_color_by
      } else {
        link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(color = I(!!sym(paste0("node.", node_color_by))))
        # Map node_color_by from nodes data to links data
        df_edges[[paste0("node.", node_color_by)]] <- get_palette(
          levels(df_nodes[[node_color_by]]),
          palette = link_palette, palcolor = link_palcolor
        )[df_nodes[df_edges[[link_color_by]], node_color_by]]
        graph <- igraph::graph_from_data_frame(d = df_edges, vertices = df_nodes, directed = directed)
      }
    }
  }

  # Handle link weight
  if (is.numeric(link_weight_by)) {
    link_layer_args$linewidth <- link_weight_by
    link_weight_by_guide <- "none"
  } else {
    link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(linewidth = !!sym(link_weight_by))
    link_weight_by_guide <- "guide"
  }

  # Handle link type
  if (!link_type_by %in% colnames(df_edges)) {
    link_layer_args$linetype <- link_type_by
    link_type_by_guide <- "none"
  } else {
    if (utils::compareVersion(as.character(utils::packageVersion("ggplot2")), "4.0.0") < 0) {
      link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(linetype = factor(!!sym(link_type_by)))
      link_type_by_guide <- "guide"
    } else {
      warning("Using `link_type_by` with ggplot2 >= 4.0.0 (ggraph <= 2.2.2) is not supported. Set to 'solid'. See https://github.com/thomasp85/ggraph/issues/394 for details.", call. = FALSE)
      link_layer_args$linetype <- "solid"
      link_type_by_guide <- "none"
    }
  }

  # Add arrow offset for directed graphs
  if (isTRUE(directed)) {
    link_layer_args$end_cap <- ggraph::circle(link_arrow_offset, "pt")
  }

  # Prepare link loop layer arguments
  link_loop_layer_args <- link_layer_args
  link_loop_layer_args$mapping[[length(link_loop_layer_args$mapping) + 1]] <- aes(direction = (!!sym("from") - 1) * 360 / length(graph))
  link_layer_args$strength <- link_curvature

  # Get modify_list from ggplot2 namespace (internal function)
  modify_list <- utils::getFromNamespace("modify_list", "ggplot2")

  # Reduce mappings to single aes() call
  link_layer_args$mapping <- Reduce(modify_list, link_layer_args$mapping)
  link_loop_layer_args$mapping <- Reduce(modify_list, link_loop_layer_args$mapping)

  # Start building the plot with ggraph
  p <- ggraph::ggraph(graph, layout = "manual", x = df_nodes$x, y = df_nodes$y)

  # Handle clustering
  if (cluster != "none") {
    clfun <- getFromNamespace(paste0("cluster_", cluster), "igraph")
    clusters <- clfun(graph)
    df_nodes$cluster <- factor(
      paste0("c", clusters$membership),
      levels = paste0("c", unique(sort(clusters$membership)))
    )

    # Override aesthetic based on cluster_scale
    if (cluster_scale == "fill") {
      if (!identical(node_fill_by, "grey20")) {
        warning("`cluster_scale = 'fill'` overrides `node_fill_by` when 'cluster' is enabled", call. = FALSE)
      }
      node_fill_by <- "cluster"
    } else if (cluster_scale == "color") {
      if (!identical(node_color_by, "black")) {
        warning("`cluster_scale = 'color'` overrides `node_color_by` when 'cluster' is enabled", call. = FALSE)
      }
      node_color_by <- "cluster"
    } else if (cluster_scale == "shape") {
      if (!identical(node_shape_by, 21)) {
        warning("`cluster_scale = 'shape'` overrides `node_shape_by` when 'cluster' is enabled", call. = FALSE)
      }
      node_shape_by <- "cluster"
    }

    # Add cluster marks if requested
    if (isTRUE(add_mark)) {
      mark_type <- match.arg(mark_type)
      mark_fun <- switch(mark_type,
        hull = ggforce::geom_mark_hull,
        ellipse = ggforce::geom_mark_ellipse,
        rect = ggforce::geom_mark_rect,
        circle = ggforce::geom_mark_circle
      )
      p <- p + mark_fun(
        data = df_nodes,
        mapping = aes(x = !!sym("x"), y = !!sym("y"), color = !!sym("cluster"), fill = !!sym("cluster")),
        expand = mark_expand, alpha = mark_alpha, linetype = mark_linetype,
        show.legend = FALSE
      ) +
        scale_fill_manual(values = get_palette(levels(df_nodes$cluster), palette = palette, palcolor = palcolor)) +
        scale_color_manual(values = get_palette(levels(df_nodes$cluster), palette = palette, palcolor = palcolor)) +
        ggnewscale::new_scale_fill() +
        ggnewscale::new_scale_color()
    }
  }

  # Add link layers
  p <- p +
    do.call(ggraph::geom_edge_arc, link_layer_args) +
    do.call(ggraph::geom_edge_loop, link_loop_layer_args)

  # Add link weight scale
  if (link_weight_by_guide == "guide") {
    p <- p + ggraph::scale_edge_width_continuous(
      range = link_weight_range, breaks = scales::pretty_breaks(n = 4),
      guide = ggplot2::guide_legend(title = link_weight_name %||% link_weight_by, order = 10)
    )
  }

  # Add link type scale
  if (link_type_by_guide == "guide") {
    p <- p + ggraph::scale_edge_linetype_discrete(
      guide = ggplot2::guide_legend(title = link_type_name %||% link_type_by, order = 11)
    )
  }

  # Add link color scale
  if (link_color_by_guide == "guide") {
    if (is.numeric(df_edges[[link_color_by]])) {
      p <- p + ggraph::scale_edge_color_gradientn(
        n.breaks = 5,
        colors = get_palette(palette = link_palette, palcolor = link_palcolor),
        na.value = "grey80",
        guide = ggplot2::guide_colorbar(
          title = link_color_name %||% link_color_by,
          frame.colour = "black", ticks.colour = "black",
          frame.linewidth = 0.3, ticks.linewidth = 0.3, title.hjust = 0, order = 12
        )
      )
    } else {
      lc_values <- if (is.factor(df_edges[[link_color_by]])) {
        levels(df_edges[[link_color_by]])
      } else {
        unique(df_edges[[link_color_by]])
      }
      p <- p + ggraph::scale_edge_color_manual(
        values = get_palette(lc_values, palette = link_palette, palcolor = link_palcolor),
        guide = ggplot2::guide_legend(title = link_color_name %||% link_color_by, order = 12)
      )
    }
  }

  # Compose and add node layer
  node_layer_args$data <- df_nodes
  node_layer_args$mapping <- Reduce(modify_list, node_layer_args$mapping)
  p <- p + do.call(ggplot2::geom_point, node_layer_args)

  # Add node size scale
  if (node_size_by_guide == "guide") {
    p <- p + ggplot2::scale_size_continuous(
      range = node_size_range, breaks = scales::pretty_breaks(n = 4),
      guide = ggplot2::guide_legend(
        title = node_size_name %||% node_size_by, order = 1,
        override.aes = list(size = scales::rescale(sort(df_nodes[[node_size_by]]), c(1, 6)))
      )
    )
  }

  # Add node color scale
  if (node_color_by_guide == "guide") {
    p <- p + ggplot2::scale_color_manual(
      values = get_palette(levels(df_nodes[[node_color_by]]), palette = palette, palcolor = palcolor),
      guide = ggplot2::guide_legend(
        title = node_color_name %||% node_color_by, order = 2,
        override.aes = list(size = 4)
      )
    )
  }

  # Add node shape scale
  if (node_shape_by_guide == "guide") {
    p <- p + ggplot2::scale_shape_manual(
      guide = ggplot2::guide_legend(
        title = node_shape_name %||% node_shape_by, order = 3,
        override.aes = list(size = 4)
      )
    )
  }

  # Add node fill scale
  if (node_fill_by_guide == "guide") {
    p <- p + ggplot2::scale_fill_manual(
      values = get_palette(levels(df_nodes[[node_fill_by]]), palette = palette, palcolor = palcolor, alpha = node_alpha),
      guide = ggplot2::guide_legend(
        title = node_fill_name %||% node_fill_by, order = 4,
        override.aes = list(size = 4)
      )
    )
  }

  # Add node labels
  if (isTRUE(add_label)) {
    p <- p + ggrepel::geom_text_repel(
      data = df_nodes, mapping = aes(x = !!sym("x"), y = !!sym("y"), label = !!sym("name")),
      segment.color = "transparent",
      point.size = NA, max.overlaps = 100, color = label_fg, bg.color = label_bg, bg.r = label_bg_r,
      size = label_size * text_size_scale
    )
  }

  # Add scales and labels
  p <- p +
    ggplot2::scale_x_continuous(expand = c(0, .4)) +
    ggplot2::scale_y_continuous(expand = c(0, .4)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab %||% "", y = ylab %||% "")

  # Apply theme
  p <- p + do.call(theme, theme_args)

  # Apply ggforge style theme for network plots
  # Note: Network plots use continuous coordinate system, so we pass NULL for vars
  axis_size <- 10 * base_size / 12
  p <- p + ggplot2::theme(
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction,
    legend.key.size = ggplot2::unit(0.5, "cm"),
    legend.key.width = ggplot2::unit(0.8, "cm"),
    axis.text.x = ggplot2::element_text(size = axis_size, colour = "black"),
    axis.text.y = ggplot2::element_text(size = axis_size, colour = "black")
  )

  # Set height and width attributes for optimal plot size
  height <- width <- 5

  if (!identical(legend.position, "none")) {
    if (legend.position %in% c("right", "left")) {
      width <- width + 1
    } else if (legend.direction == "horizontal") {
      height <- height + 1
    } else {
      height <- height + 2
    }
  }

  attr(p, "height") <- height
  attr(p, "width") <- width

  return(p)
}
