#' Adjust Network Layout
#'
#' @description
#' Internal helper function for adjusting network layout to prevent node overlap
#'
#' @param graph igraph object
#' @param layout Initial layout matrix
#' @param width Width values for nodes
#' @param height Height for vertical spacing
#' @param scale Scaling factor for layout
#' @param iter Number of iterations for adjustment
#' @return Adjusted layout matrix
#' @keywords internal
#' @importFrom stats dist
adjust_network_layout <- function(graph, layout, width, height = 2, scale = 100, iter = 100) {
  w <- width / 2
  layout[, 1] <- layout[, 1] / diff(range(layout[, 1])) * scale
  layout[, 2] <- layout[, 2] / diff(range(layout[, 2])) * scale

  adjusted <- c()
  # for (i in seq_len(iter)) {
  for (v in order(igraph::degree(graph), decreasing = TRUE)) {
    adjusted <- c(adjusted, v)
    neighbors <- as.numeric(igraph::neighbors(graph, igraph::V(graph)[v]))
    neighbors <- setdiff(neighbors, adjusted)
    x <- layout[v, 1]
    y <- layout[v, 2]
    r <- w[v]
    for (neighbor in neighbors) {
      nx <- layout[neighbor, 1]
      ny <- layout[neighbor, 2]
      ndist <- sqrt((nx - x)^2 + (ny - y)^2)
      nr <- w[neighbor]
      expect <- r + nr
      if (ndist < expect) {
        dx <- (x - nx) * (expect - ndist) / ndist
        dy <- (y - ny) * (expect - ndist) / ndist
        layout[neighbor, 1] <- nx - dx
        layout[neighbor, 2] <- ny - dy
        adjusted <- c(adjusted, neighbor)
      }
    }
  }
  # }

  for (i in seq_len(iter)) {
    dist_matrix <- as.matrix(dist(layout))
    nearest_neighbors <- apply(dist_matrix, 2, function(x) which(x == min(x[x > 0])), simplify = FALSE)
    # nearest_neighbors <- apply(dist_matrix, 2, function(x) {
    #   head(order(x), 3)[-1]
    # }, simplify = FALSE)
    for (v in sample(seq_len(nrow(layout)))) {
      neighbors <- unique(nearest_neighbors[[v]])
      x <- layout[v, 1]
      y <- layout[v, 2]
      r <- w[v]
      for (neighbor in neighbors) {
        nx <- layout[neighbor, 1]
        ny <- layout[neighbor, 2]
        nr <- w[neighbor]
        if (abs(nx - x) < (r + nr) && abs(ny - y) < height) {
          dx <- r + nr - (nx - x)
          dy <- height - (ny - y)
          if (sample(c(1, 0), 1) == 1) {
            dx <- 0
          } else {
            dy <- 0
          }
          layout[neighbor, 1] <- nx - dx
          layout[neighbor, 2] <- ny - dy
        }
      }
    }
  }
  return(layout)
}

#' Process the enrichment results from Enrichr
#'
#' @description
#' Converts Enrichr format to clusterProfiler-compatible format
#'
#' @param data A data frame containing the result by Enrichr.
#' @param dbname A character string specifying the name of the database column.
#' @param n_input An integer specifying the number of input genes.
#'   Enrichr result doesn't ship with the number of input genes.
#'   You can either provide the number directly or we will infer it. See details.
#' @return A data frame that can be used in `EnrichMap`.
#' @keywords internal
#' @details
#' In order to use the `EnrichMap` and `EnrichNetwork` functions and other visualization functions in `ggforge`,
#' the enrichment results from Enrichr need to be processed by the `prepare_enrichr_result` function.
#' The following columns are renamed:
#' * `Term` -> `Description`
#' * `Genes` -> `geneID` (separated replaced by `/`)
#' * `P.value` -> `pvalue`
#' * `Adjusted.P.value` -> `p.adjust`
#' Additionally, GeneRatio and BgRatio columns are inferred.
#' From [enrichr's documentation](https://maayanlab.cloud/Enrichr/help#background), the oddsRatio is defined as:
#' `oddsRatio = (A * (D - B - C + A) / max((B - A) * (C - A), 1)`, where A is the overlapping genes; B is the total genes in the gene set;
#' C (n_input) is the genes in input list; D is the total genes in the background.
#' D is not provided by Enrichr. To infer it, `D = oddsRatio * max((B - A) * (C - A), 1) / A + B + C - A`.
#' * `Overlap = A / B` (from Enrichr)
#' * `GeneRatio = A / C` (from ClusterProfiler)
#' * `BgRatio = B / D` (from ClusterProfiler)
#' `C (n_input)`, if not provided, will be inferred when `D` for all terms are equal.
#' When starting inferrence, the minimum value to try will be unique genes in `data$Genes`/`data$geneID`.
prepare_enrichr_result <- function(data, dbname = "Database", n_input = NULL) {
  # Handle list input recursively
  if (inherits(data, "list")) {
    data <- lapply(names(data), function(x) {
      df <- prepare_enrichr_result(data[[x]])
      df[[dbname]] <- x
      df
    })
    return(do.call(rbind, data))
  }

  # Calculate GeneRatio and BgRatio
  A_B <- strsplit(data$Overlap, "/", fixed = TRUE)
  A <- as.numeric(sapply(A_B, `[`, 1))
  B <- as.numeric(sapply(A_B, `[`, 2))
  data$geneID <- data$Genes
  data$Genes <- NULL
  data$geneID <- gsub(";", "/", data$geneID, fixed = TRUE)

  if (is.null(n_input)) {
    # Infer n_input by iterating until D values are consistent
    C <- length(unique(unlist(strsplit(unlist(data$geneID), "/", fixed = TRUE))))
    while (TRUE) {
      D <- data$Odds.Ratio * pmax((B - A) * (C - A), 1) / A + B + C - A
      if (isTRUE(all.equal(D, rep(D[1], length(D))))) {
        break
      }
      if (D[1] > 100000) {
        stop("Failed to infer the number of input genes. Please provide it manually.", call. = FALSE)
      }
      C <- C + 1
    }
  } else {
    C <- n_input
    D <- data$Odds.Ratio * pmax((B - A) * (C - A), 1) / A + B + C - A
  }

  D <- round(D)
  data$Count <- A
  data$GeneRatio <- paste(A, C, sep = "/")
  data$BgRatio <- paste(B, D, sep = "/")
  data$Overlap <- NULL
  data$Odds.Ratio <- NULL
  data$Description <- data$Term
  data$Term <- NULL
  data$pvalue <- data[["P.value"]]
  data[["P.value"]] <- NULL
  data$p.adjust <- data[["Adjusted.P.value"]]
  data[["Adjusted.P.value"]] <- NULL

  data
}

#' Atomic Enrichment Map
#'
#' @description
#' Creates a single enrichment map plot showing clustering of enriched terms
#'
#' @inheritParams parameters
#' @param data A data frame containing the data to be plotted.
#'   It should be in the format of clusterProfiler enrichment result,
#'   which includes the columns: ID, Description, GeneRatio, BgRatio, pvalue, p.adjust,
#'   qvalue, geneID and Count.
#'   * The `ID`, `qvalue`, `BgRatio`, and `Count` columns are optional.
#'   * The `Description` is the description of the term.
#'   * The `GeneRatio` is the number of genes in the term divided by the total number of genes in the input list.
#'   * The `BgRatio` is the number of genes in the term divided by the total number of genes in the background list (all terms).
#'   * The `Count` column, if given, should be the same as the first number in GeneRatio.
#' @param in_form Input format ("clusterProfiler" or "enrichr")
#' @param top_term An integer specifying the number of top terms to show.
#' @param metric A character string specifying the metric to use for the size of the nodes.
#'   It is also used to order the terms when selected the top terms.
#'   Either "pvalue" or "p.adjust". The default is "p.adjust".
#' @param layout A character string specifying the layout of the graph.
#'   Either "circle", "tree", "grid" or other layout functions in `igraph`.
#' @param minchar An integer specifying the minimum number of characters to show in the keyword.
#' @param cluster A character string specifying the clustering method.
#'   Either "fast_greedy", "walktrap", "edge_betweenness", "infomap" or other clustering functions in `igraph`.
#' @param show_keyword A logical value specifying whether to show the keyword instead of Description/Term in the plot.
#' @param nlabel An integer specifying the number of labels to show in each cluster.
#' @param character_width The width of the characters used to wrap the keyword.
#' @param mark A character string specifying the mark to use for the nodes.
#'   Either "ellipse", "rect", "circle", "text" or other mark functions in `ggforce`.
#' @param label A character string specifying the label to show in the legend.
#'   Either "term" or "feature". The default is "term".
#' @param labelsize A numeric value specifying the size of the label.
#' @param expand A numeric vector of length 2 specifying the expansion of the x and y axis.
#' @param words_excluded A character vector specifying the words to exclude in the keyword.
#' @return A ggplot object
#' @keywords internal
#' @importFrom utils combn
#' @importFrom dplyr slice_min slice_head distinct group_by arrange desc mutate filter reframe
#' @importFrom scales breaks_extended
#' @importFrom ggplot2 geom_segment geom_point labs scale_size guides scale_linewidth scale_fill_manual scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous guide_legend guide_none unit margin aes
#' @importFrom rlang sym syms "%||%"
#' @importFrom tidyr unnest
#' @importFrom stringr str_wrap
EnrichMapAtomic <- function(
    data, in_form = "clusterProfiler", top_term = 100, metric = "p.adjust", layout = "fr", minchar = 2,
    cluster = "fast_greedy", show_keyword = FALSE, nlabel = 4, character_width = 50,
    words_excluded = NULL,
    mark = "ellipse", label = c("term", "feature"), labelsize = 5, expand = c(0.4, 0.4),
    theme = "theme_ggforge", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525, ...) {
  # 1. Get ggplot function (support gglogger)
  ggplot <- get_ggplot()

  # 2. Load words_excluded if not provided
  if (is.null(words_excluded)) {
    words_excluded <- get("words_excluded", envir = asNamespace("ggforge"))
  }

  # 3. Convert enrichResult to data.frame if needed
  if (inherits(data, "enrichResult")) {
    data <- as.data.frame(data)
  }

  # 4. Process Enrichr format if needed
  if (in_form == "enrichr") {
    data <- prepare_enrichr_result(data)
  }

  # 5. Validate columns
  data$Description <- validate_columns(data, "Description", force_factor = TRUE)
  validate_columns(data, "GeneRatio")
  validate_columns(data, "pvalue")
  validate_columns(data, "p.adjust")
  validate_columns(data, "geneID")

  # 6. Process label and expand parameters
  label <- match.arg(label)
  expand <- normalize_expansion(expand, x_type = "continuous", y_type = "continuous")

  # 7. Select top terms if specified
  if (!is.null(top_term)) {
    data <- dplyr::slice_min(data, !!sym(metric), n = top_term, with_ties = FALSE)
  }

  # 8. Prepare data for network analysis
  data$metric <- -log10(data[[metric]])
  data$geneID <- strsplit(data$geneID, "/")

  # Create IDs if not present
  if (is.null(data$ID) && is.null(rownames(data))) {
    data$ID <- paste0("GS", seq_len(nrow(data)))
  } else if (is.null(data$ID)) {
    data$ID <- rownames(data)
  }

  # 9. Build graph from term overlaps
  nodes <- data
  edges <- as.data.frame(t(utils::combn(nodes$ID, 2)))
  colnames(edges) <- c("from", "to")

  # Calculate edge weights based on gene overlap
  edges$weight <- mapply(
    function(x, y) length(intersect(unlist(data[x, "geneID"]), unlist(data[y, "geneID"]))),
    edges$from,
    edges$to
  )
  edges <- edges[edges$weight > 0, , drop = FALSE]

  nodes <- nodes[c("ID", setdiff(colnames(nodes), "ID"))]
  graph <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

  # 10. Calculate layout
  if (layout %in% c("circle", "tree", "grid")) {
    layout <- switch(layout,
      "circle" = igraph::layout_in_circle(graph),
      "tree" = igraph::layout_as_tree(graph),
      "grid" = igraph::layout_on_grid(graph)
    )
  } else {
    lofun <- utils::getFromNamespace(paste0("layout_with_", layout), "igraph")
    layout <- lofun(graph)
  }

  # 11. Cluster nodes
  clfun <- utils::getFromNamespace(paste0("cluster_", cluster), "igraph")
  clusters <- clfun(graph)

  # 12. Prepare node data with coordinates and clusters
  df_graph <- igraph::as_data_frame(graph, what = "both")
  df_nodes <- df_graph$vertices
  df_nodes$dim1 <- layout[, 1]
  df_nodes$dim2 <- layout[, 2]
  df_nodes$clusters <- factor(
    paste0("C", clusters$membership),
    paste0("C", unique(sort(clusters$membership)))
  )

  # 13. Extract keywords from term descriptions
  if (isTRUE(show_keyword)) {
    # Extract keywords from Description text
    df_keyword1 <- df_nodes %>%
      dplyr::mutate(keyword = strsplit(tolower(as.character(!!sym("Description"))), "\\s|\\n", perl = TRUE)) %>%
      tidyr::unnest(cols = "keyword") %>%
      dplyr::group_by(!!sym("keyword"), !!sym("clusters")) %>%
      dplyr::reframe(
        keyword = tools::toTitleCase(!!sym("keyword")),
        score = sum(!!sym("metric")),
        count = n(),
        .groups = "keep"
      ) %>%
      dplyr::filter(!grepl(pattern = "\\[.*\\]", x = !!sym("keyword"))) %>%
      dplyr::filter(nchar(!!sym("keyword")) >= minchar) %>%
      dplyr::filter(!tolower(!!sym("keyword")) %in% tolower(words_excluded)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(!!sym("clusters")) %>%
      dplyr::arrange(dplyr::desc(!!sym("score"))) %>%
      dplyr::slice_head(n = nlabel) %>%
      dplyr::reframe(keyword = paste0(!!sym("keyword"), collapse = "/")) %>%
      as.data.frame()

    rownames(df_keyword1) <- as.character(df_keyword1$clusters)
    df_keyword1$keyword <- stringr::str_wrap(df_keyword1$keyword, width = character_width)
    df_keyword1$label <- paste0(df_keyword1$clusters, ":\n", df_keyword1$keyword)
  } else {
    # Use full term descriptions
    df_nodes$Description <- paste0("- ", stringr::str_wrap(df_nodes$Description, width = character_width, exdent = 2))

    df_keyword1 <- df_nodes %>%
      dplyr::group_by(!!sym("clusters")) %>%
      dplyr::arrange(dplyr::desc(!!sym("metric"))) %>%
      dplyr::reframe(keyword = !!sym("Description")) %>%
      dplyr::distinct() %>%
      dplyr::group_by(!!sym("clusters")) %>%
      dplyr::slice_head(n = nlabel) %>%
      dplyr::reframe(keyword = paste0(!!sym("keyword"), collapse = "\n")) %>%
      as.data.frame()

    rownames(df_keyword1) <- as.character(df_keyword1$clusters)
    df_keyword1$label <- paste0(df_keyword1$clusters, ":\n", df_keyword1$keyword)
  }

  # 14. Extract gene-level keywords for feature labels
  df_keyword2 <- df_nodes %>%
    dplyr::mutate(keyword = !!sym("geneID")) %>%
    tidyr::unnest(cols = "keyword") %>%
    dplyr::group_by(!!sym("keyword"), !!sym("clusters")) %>%
    dplyr::reframe(
      keyword = !!sym("keyword"),
      score = sum(!!sym("metric")),
      count = n(),
      .groups = "keep"
    ) %>%
    dplyr::distinct() %>%
    dplyr::group_by(!!sym("clusters")) %>%
    dplyr::arrange(dplyr::desc(!!sym("score"))) %>%
    dplyr::slice_head(n = nlabel) %>%
    dplyr::reframe(keyword = paste0(!!sym("keyword"), collapse = "|")) %>%
    as.data.frame()

  rownames(df_keyword2) <- as.character(df_keyword2$clusters)
  df_keyword2$keyword <- stringr::str_wrap(df_keyword2$keyword, width = character_width)
  df_keyword2$label <- paste0(df_keyword2$clusters, ":\n", df_keyword2$keyword)

  df_nodes$keyword1 <- df_keyword1[as.character(df_nodes$clusters), "keyword"]
  df_nodes$keyword2 <- df_keyword2[as.character(df_nodes$clusters), "keyword"]

  # 15. Prepare edge data with coordinates
  df_edges <- df_graph$edges
  df_edges$from_dim1 <- df_nodes[df_edges$from, "dim1"]
  df_edges$from_dim2 <- df_nodes[df_edges$from, "dim2"]
  df_edges$to_dim1 <- df_nodes[df_edges$to, "dim1"]
  df_edges$to_dim2 <- df_nodes[df_edges$to, "dim2"]

  # 16. Get colors for clusters
  colors <- get_palette(
    levels(df_nodes$clusters),
    palette = palette,
    palcolor = palcolor,
    alpha = alpha
  )

  # 17. Create mark layer for cluster highlighting
  markfun <- utils::getFromNamespace(paste0("geom_mark_", mark), "ggforce")
  mark_layer <- markfun(
    data = df_nodes, aes(
      x = !!sym("dim1"), y = !!sym("dim2"), color = !!sym("clusters"), fill = !!sym("clusters"),
      label = !!sym("clusters"), description = !!sym(ifelse(label == "term", "keyword1", "keyword2"))
    ),
    expand = ggplot2::unit(3, "mm"),
    alpha = 0.1,
    label.margin = ggplot2::margin(1, 1, 1, 1, "mm"),
    label.fontsize = labelsize * 2,
    label.fill = "grey95",
    label.minwidth = ggplot2::unit(character_width, "in"),
    label.buffer = ggplot2::unit(0, "mm"),
    con.size = 1,
    con.cap = 0
  )

  # 18. Build ggplot
  p <- ggplot() +
    mark_layer +
    ggplot2::geom_segment(
      data = df_edges,
      aes(x = !!sym("from_dim1"), y = !!sym("from_dim2"), xend = !!sym("to_dim1"), yend = !!sym("to_dim2"), linewidth = !!sym("weight")),
      alpha = 0.1, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = df_nodes,
      aes(x = !!sym("dim1"), y = !!sym("dim2"), size = !!sym("Count"), fill = !!sym("clusters")),
      color = "black", shape = 21
    ) +
    labs(title = title, subtitle = subtitle, x = xlab %||% "", y = ylab %||% "") +
    ggplot2::scale_size(name = "Count", range = c(2, 6), breaks = scales::breaks_extended(n = 4)) +
    ggplot2::guides(size = ggplot2::guide_legend(override.aes = list(fill = "grey30", shape = 21), order = 1)) +
    ggplot2::scale_linewidth(name = "Intersection", range = c(0.3, 3), breaks = scales::breaks_extended(n = 4)) +
    ggplot2::guides(linewidth = ggplot2::guide_legend(override.aes = list(alpha = 1, color = "grey"), order = 2)) +
    ggplot2::scale_fill_manual(
      name = switch(label,
        "term" = "Feature",
        "feature" = "Term"
      ),
      values = colors,
      labels = if (label == "term") df_keyword2[levels(df_nodes$clusters), "label"] else df_keyword1[levels(df_nodes$clusters), "label"],
      na.value = "grey80",
      aesthetics = c("colour", "fill")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      override.aes = list(alpha = 1, color = "black", shape = NA),
      byrow = TRUE,
      theme = ggplot2::theme(legend.key.spacing.y = ggplot2::unit(0.1, "cm")),
      order = 3
    )) +
    ggplot2::guides(color = ggplot2::guide_none()) +
    ggplot2::scale_x_continuous(expand = expand$x) +
    ggplot2::scale_y_continuous(expand = expand$y)

  # 19. Apply theme
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(aspect.ratio = aspect.ratio)

  # 20. Apply data-driven styling (no specific x/y vars for network plots)
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = NULL,
    y_var = NULL,
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # 21. Set recommended plot dimensions
  height <- width <- 8
  if (!identical(legend.position, "none")) {
    if (legend.position %in% c("right", "left")) {
      width <- width + 2
    } else if (legend.direction == "horizontal") {
      height <- height + 1
    } else {
      width <- width + 4
    }
  }
  attr(p, "height") <- height
  attr(p, "width") <- width

  return(p)
}

#' Atomic Enrichment Network
#'
#' @description
#' Creates a single enrichment network plot showing connections between terms and genes
#'
#' @inheritParams EnrichMapAtomic
#' @param layoutadjust A logical value specifying whether to adjust the layout of the network.
#' @param adjscale A numeric value specifying the scale of the adjustment.
#' @param adjiter A numeric value specifying the number of iterations for the adjustment.
#' @param blendmode A character string specifying the blend mode of the colors.
#'   Either "blend", "average", "multiply" and "screen".
#' @return A ggplot object
#' @keywords internal
#' @importFrom utils getFromNamespace
#' @importFrom grDevices col2rgb
#' @importFrom ggplot2 scale_color_identity scale_fill_identity guides guide_legend draw_key_point .pt element_text geom_label aes
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang sym "%||%"
EnrichNetworkAtomic <- function(
    data, top_term = 6, metric = "p.adjust", character_width = 50,
    layout = "fr", layoutadjust = TRUE, adjscale = 60, adjiter = 100, blendmode = "blend", labelsize = 5,
    theme = "theme_ggforge", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    ...) {
  # 1. Get ggplot function (support gglogger)
  ggplot <- get_ggplot()

  # 2. Convert enrichResult to data.frame if needed
  if (inherits(data, "enrichResult")) {
    data <- as.data.frame(data)
  }

  # 3. Validate columns
  validate_columns(data, "Description", force_factor = TRUE)
  validate_columns(data, "GeneRatio")
  validate_columns(data, "pvalue")
  validate_columns(data, "p.adjust")
  validate_columns(data, "geneID")

  # 4. Select top terms if specified
  if (!is.null(top_term)) {
    data <- dplyr::slice_min(data, !!sym(metric), n = top_term, with_ties = FALSE)
  }

  # 5. Prepare data for network
  data$metric <- -log10(data[[metric]])
  data$Description <- stringr::str_wrap(data$Description, width = character_width)
  data$Description <- factor(data$Description, levels = unique(data$Description))
  data$geneID <- strsplit(data$geneID, "/")
  df_unnest <- tidyr::unnest(data, cols = "geneID")

  # 6. Build bipartite network (terms and genes as nodes)
  nodes <- rbind(
    data.frame("ID" = data$Description, class = "term", metric = data$metric),
    data.frame("ID" = unique(df_unnest$geneID), class = "gene", metric = 0)
  )
  nodes$Database <- data$Database[1]

  edges <- as.data.frame(df_unnest[, c("Description", "geneID")])
  colnames(edges) <- c("from", "to")
  edges$weight <- 1

  graph <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

  # 7. Calculate layout
  if (layout %in% c("circle", "tree", "grid")) {
    layout <- switch(layout,
      "circle" = igraph::layout_in_circle(graph),
      "tree" = igraph::layout_as_tree(graph),
      "grid" = igraph::layout_on_grid(graph)
    )
  } else {
    lofun <- utils::getFromNamespace(paste0("layout_with_", layout), "igraph")
    layout <- lofun(graph)
  }

  df_graph <- igraph::as_data_frame(graph, what = "both")
  df_nodes <- df_graph$vertices

  # 8. Adjust layout to prevent node overlap
  if (isTRUE(layoutadjust)) {
    width <- nchar(df_nodes$name)
    width[df_nodes$class == "term"] <- 8
    layout <- adjust_network_layout(
      graph = graph, layout = layout, width = width, height = 2,
      scale = adjscale, iter = adjiter
    )
  }

  df_nodes$dim1 <- layout[, 1]
  df_nodes$dim2 <- layout[, 2]

  # 9. Prepare edge data with coordinates
  df_edges <- df_graph$edges
  df_edges$from_dim1 <- df_nodes[df_edges$from, "dim1"]
  df_edges$from_dim2 <- df_nodes[df_edges$from, "dim2"]
  df_edges$to_dim1 <- df_nodes[df_edges$to, "dim1"]
  df_edges$to_dim2 <- df_nodes[df_edges$to, "dim2"]

  # 10. Generate colors for terms
  colors <- get_palette(
    levels(data$Description),
    palette = palette,
    palcolor = palcolor
  )
  df_edges$color <- colors[df_edges$from]

  # 11. Blend colors for genes (connected to multiple terms)
  node_colors <- aggregate(
    df_unnest$Description,
    by = list(df_unnest$geneID),
    FUN = function(x) blend_colors(colors = colors[x], mode = blendmode)
  )
  colors <- c(colors, setNames(node_colors[, 2], node_colors[, 1]))
  label_colors <- ifelse(colSums(grDevices::col2rgb(colors)) > 255 * 2, "black", "white")

  df_nodes$color <- colors[df_nodes$name]
  df_nodes$label_color <- label_colors[df_nodes$name]
  df_nodes$label <- NA
  df_nodes[levels(data$Description), "label"] <- seq_len(nlevels(data$Description))

  # 12. Create custom legend key drawing function
  draw_key_cust <- function(df, params, size) {
    data_text <- df
    data_text$label <- which(levels(data$Description) %in% names(colors)[colors == data_text$fill])
    data_text$colour <- "black"
    data_text$alpha <- 1
    data_text$size <- 11 / ggplot2::.pt
    grid::grobTree(
      ggplot2::draw_key_point(df, list(color = "white", shape = 21)),
      utils::getFromNamespace("shadowtextGrob", "ggrepel")(
        label = data_text$label,
        bg.colour = "black",
        bg.r = 0.1,
        gp = grid::gpar(col = "white", fontface = "bold")
      )
    )
  }

  # 13. Build ggplot
  p <- ggplot() +
    ggplot2::geom_segment(
      data = df_edges,
      aes(x = !!sym("from_dim1"), y = !!sym("from_dim2"), xend = !!sym("to_dim1"), yend = !!sym("to_dim2"), color = !!sym("color")),
      alpha = 1, lineend = "round", show.legend = FALSE
    ) +
    ggplot2::geom_label(
      data = df_nodes[df_nodes$class == "gene", ],
      aes(x = !!sym("dim1"), y = !!sym("dim2"), label = !!sym("name"), fill = !!sym("color"), color = !!sym("label_color")),
      size = 3, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = df_nodes[df_nodes$class == "term", ],
      aes(x = !!sym("dim1"), y = !!sym("dim2")),
      size = 8, color = "black", fill = "black", stroke = 1, shape = 21, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = df_nodes[df_nodes$class == "term", ],
      aes(x = !!sym("dim1"), y = !!sym("dim2"), fill = !!sym("color")),
      size = 7, color = "white", stroke = 1, shape = 21, key_glyph = draw_key_cust
    ) +
    ggrepel::geom_text_repel(
      data = df_nodes[df_nodes$class == "term", ],
      aes(x = !!sym("dim1"), y = !!sym("dim2"), label = !!sym("label")),
      fontface = "bold", min.segment.length = 0, segment.color = "black",
      point.size = NA, max.overlaps = 100, force = 0, color = "white", bg.color = "black", bg.r = 0.1, size = labelsize
    ) +
    ggplot2::scale_color_identity(guide = "none") +
    ggplot2::scale_fill_identity(
      name = "Term", guide = "legend",
      labels = levels(data$Description),
      breaks = colors[levels(data$Description)]
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(color = "transparent"))) +
    labs(title = title, subtitle = subtitle, x = xlab %||% "", y = ylab %||% "")

  # 14. Apply theme
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- p +
    do.call(theme, theme_args) +
    ggplot2::theme(aspect.ratio = aspect.ratio)

  # 15. Apply data-driven styling (no specific x/y vars for network plots)
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = NULL,
    y_var = NULL,
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # 16. Set recommended plot dimensions
  height <- width <- 8
  if (!identical(legend.position, "none")) {
    if (legend.position %in% c("right", "left")) {
      width <- width + 2
    } else if (legend.direction == "horizontal") {
      height <- height + 1
    } else {
      width <- width + 4
    }
  }
  attr(p, "height") <- height
  attr(p, "width") <- width

  return(p)
}

#' Enrichment Map
#'
#' @description
#' Creates enrichment map visualizations showing clustering of enriched terms
#' based on gene overlap. Terms are nodes, and edges represent shared genes.
#' Nodes are clustered to reveal functional modules.
#'
#' @inheritParams parameters
#' @inheritParams EnrichMapAtomic
#' @param data A data frame containing the data to be plotted.
#'   It should be in the format of clusterProfiler enrichment result,
#'   which includes the columns: ID, Description, GeneRatio, BgRatio, pvalue, p.adjust,
#'   qvalue, geneID and Count.
#'   * The `ID`, `qvalue` and `Count` columns are optional.
#'   * The `Description` is the description of the term.
#'   * The `GeneRatio` is the number of genes in the term divided by the total number of genes in the input list.
#'   * The `BgRatio` is the number of genes in the term divided by the total number of genes in the background list (all terms).
#'   * The `Count` column, if given, should be the same as the first number in GeneRatio.
#'
#'   If you have enrichment results from multiple databases, you can combine them into one data frame and add a column (e.g. Database)
#'   to indicate the database.
#'   You can plot them in a single plot using the `split_by` argument (e.g. `split_by = "Database"`).
#' @param in_form A character string specifying the input format.
#'   Either "auto", "clusterProfiler", "clusterprofiler" or "enrichr".
#'   The default is "auto", which will try to infer the input format.
#'
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' \donttest{
#' data(enrich_example)
#' EnrichMap(enrich_example)
#' EnrichMap(enrich_example, label = "feature")
#' EnrichMap(enrich_example, show_keyword = TRUE, label = "term")
#' EnrichMap(enrich_example, show_keyword = TRUE, label = "feature")
#'
#' data(enrich_multidb_example)
#' EnrichMap(enrich_multidb_example, split_by = "Database")
#' EnrichMap(enrich_multidb_example,
#'   split_by = "Database",
#'   palette = list(DB1 = "Paired", DB2 = "Set1")
#' )
#' }
EnrichMap <- function(
    data, in_form = c("auto", "clusterProfiler", "clusterprofiler", "enrichr"), split_by = NULL,
    split_by_sep = "_", top_term = 10, metric = "p.adjust", layout = "fr", minchar = 2,
    cluster = "fast_greedy", show_keyword = FALSE, nlabel = 4, character_width = 50,
    mark = "ellipse", label = c("term", "feature"), labelsize = 5, expand = c(0.4, 0.4),
    words_excluded = NULL,
    theme = "theme_ggforge", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...) {
  # 1. Validate common arguments
  validate_common_args(seed = seed)

  # 2. Process theme
  theme <- process_theme(theme)

  # 3. Determine input format
  in_form <- match.arg(in_form)
  in_form <- tolower(in_form)
  if (in_form == "auto") {
    if (all(c("pvalue", "p.adjust", "qvalue") %in% colnames(data))) {
      in_form <- "clusterprofiler"
    } else if (all(c("P.value", "Adjusted.P.value") %in% colnames(data))) {
      in_form <- "enrichr"
    } else {
      stop("Cannot infer the input format. Please provide it manually.", call. = FALSE)
    }
  }

  # 4. Convert Enrichr format if needed
  if (in_form == "enrichr") {
    data <- prepare_enrichr_result(data)
  }

  # 5. Validate split_by
  split_by <- validate_columns(data, split_by,
    force_factor = TRUE, allow_multi = TRUE,
    concat_multi = TRUE, concat_sep = split_by_sep
  )

  # 6. Collect all parameters for passing to atomic function
  params <- as.list(environment())
  params$data <- NULL # Remove data from params

  # 7. Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = EnrichMapAtomic,
    params = params,
    split_by = split_by,
    facet_by = NULL,
    combine = combine
  )
}

#' Enrichment Network
#'
#' @description
#' Creates enrichment network visualizations showing connections between enriched
#' terms and their associated genes. Terms and genes are both shown as nodes,
#' with edges connecting terms to their genes.
#'
#' @inheritParams parameters
#' @inheritParams EnrichNetworkAtomic
#' @param data A data frame containing the data to be plotted.
#'   It should be in the format of clusterProfiler enrichment result,
#'   which includes the columns: ID, Description, GeneRatio, BgRatio, pvalue, p.adjust,
#'   qvalue, geneID and Count.
#'   * The `ID`, `qvalue` and `Count` columns are optional.
#'   * The `Description` is the description of the term.
#'   * The `GeneRatio` is the number of genes in the term divided by the total number of genes in the input list.
#'   * The `BgRatio` is the number of genes in the term divided by the total number of genes in the background list (all terms).
#'   * The `Count` column, if given, should be the same as the first number in GeneRatio.
#'
#'   If you have enrichment results from multiple databases, you can combine them into one data frame and add a column (e.g. Database)
#'   to indicate the database.
#'   You can plot them in a single plot using the `split_by` argument (e.g. `split_by = "Database"`).
#' @param in_form A character string specifying the input format.
#'   Either "auto", "clusterProfiler", "clusterprofiler" or "enrichr".
#'   The default is "auto", which will try to infer the input format.
#'
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' \donttest{
#' data(enrich_example)
#' EnrichNetwork(enrich_example, top_term = 5)
#' }
EnrichNetwork <- function(
    data, in_form = c("auto", "clusterProfiler", "clusterprofiler", "enrichr"),
    split_by = NULL, split_by_sep = "_", top_term = 10, metric = "p.adjust", character_width = 50,
    layout = "fr", layoutadjust = TRUE, adjscale = 60, adjiter = 100, blendmode = "blend", labelsize = 5,
    theme = "theme_ggforge", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
    ...) {
  # 1. Validate common arguments
  validate_common_args(seed = seed)

  # 2. Process theme
  theme <- process_theme(theme)

  # 3. Determine input format
  in_form <- match.arg(in_form)
  in_form <- tolower(in_form)
  if (in_form == "auto") {
    if (all(c("pvalue", "p.adjust", "qvalue") %in% colnames(data))) {
      in_form <- "clusterprofiler"
    } else if (all(c("P.value", "Adjusted.P.value") %in% colnames(data))) {
      in_form <- "enrichr"
    } else {
      stop("Cannot infer the input format. Please provide it manually.", call. = FALSE)
    }
  }

  # 4. Convert Enrichr format if needed
  if (in_form == "enrichr") {
    data <- prepare_enrichr_result(data)
  }

  # 5. Validate split_by
  split_by <- validate_columns(data, split_by,
    force_factor = TRUE, allow_multi = TRUE,
    concat_multi = TRUE, concat_sep = split_by_sep
  )

  # 6. Collect all parameters for passing to atomic function
  params <- as.list(environment())
  params$data <- NULL # Remove data from params

  # 7. Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = EnrichNetworkAtomic,
    params = params,
    split_by = split_by,
    facet_by = NULL,
    combine = combine
  )
}
