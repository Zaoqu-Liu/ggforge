# Network Plot Atomic

Creates a single network plot without splitting the data.

## Usage

``` r
NetworkAtomic(
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
  ...
)
```

## Arguments

- links:

  A data frame containing the links between nodes.

- nodes:

  A data frame containing the nodes (optional). Node names are extracted
  from the links data frame if not provided. If `"@nodes"` is provided,
  the nodes data frame will be extracted from the `nodes` attribute of
  the links data frame.

- from:

  Column name(s) for source nodes. Default is the first column.

- from_sep:

  Separator for concatenating multiple `from` columns.

- to:

  Column name(s) for target nodes. Default is the second column.

- to_sep:

  Separator for concatenating multiple `to` columns.

- node_by:

  Column name(s) for node identifiers. Default is the first column.

- node_by_sep:

  Separator for concatenating multiple `node_by` columns.

- link_weight_by:

  Numeric value or column name for link width.

- link_weight_name:

  Legend title for link weight.

- link_type_by:

  Link line type: "solid", "dashed", "dotted", or a column name.

- link_type_name:

  Legend title for link type.

- node_size_by:

  Numeric value or column name for node size.

- node_size_name:

  Legend title for node size.

- node_color_by:

  Color value or column name for node color (border).

- node_color_name:

  Legend title for node color.

- node_shape_by:

  Numeric value or column name for node shape.

- node_shape_name:

  Legend title for node shape.

- node_fill_by:

  Fill color value or column name for node fill.

- node_fill_name:

  Legend title for node fill.

- link_alpha:

  Transparency for links (0-1).

- node_alpha:

  Transparency for node fill (0-1).

- node_stroke:

  Width of node borders.

- cluster_scale:

  Which aesthetic to use for clusters: "fill", "color", or "shape".

- node_size_range:

  Range for node sizes.

- link_weight_range:

  Range for link widths.

- link_arrow_offset:

  Offset for link arrows to avoid overlapping nodes.

- link_curvature:

  Curvature of links (0 = straight).

- link_color_by:

  Link coloring: "from", "to", or a column name.

- link_color_name:

  Legend title for link color.

- palette:

  Color palette name

- palcolor:

  Custom colors for palette

- link_palette:

  Palette for link colors.

- link_palcolor:

  Custom colors for link palette.

- directed:

  Whether the graph is directed.

- layout:

  Layout algorithm: "circle", "tree", "grid", or an igraph layout name.

- cluster:

  Clustering method: "none", "fast_greedy", "walktrap",
  "edge_betweenness", "infomap", or an igraph clustering function.

- add_mark:

  Whether to add visual marks around clusters.

- mark_expand:

  Expansion of cluster marks.

- mark_type:

  Type of cluster marks: "hull", "ellipse", "rect", or "circle".

- mark_alpha:

  Transparency of cluster marks.

- mark_linetype:

  Line type of cluster marks.

- add_label:

  Whether to add labels to nodes.

- label_size:

  Size of node labels.

- label_fg:

  Foreground color of labels.

- label_bg:

  Background color of labels.

- label_bg_r:

  Background ratio for labels.

- arrow:

  Arrow specification for directed graphs.

- title:

  Plot title

- subtitle:

  Plot subtitle

- xlab:

  X-axis label

- ylab:

  Y-axis label

- aspect.ratio:

  Aspect ratio of plot panel

- theme:

  Theme name (string) or theme function

- theme_args:

  List of arguments passed to theme function

- legend.position:

  Legend position: "none", "left", "right", "bottom", "top"

- legend.direction:

  Legend direction: "horizontal" or "vertical"
