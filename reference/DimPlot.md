# Dimension Reduction Plot

Visualizes dimension reduction data (UMAP, t-SNE, PCA, etc.) as scatter
plots. Supports both categorical grouping and continuous feature
plotting with extensive customization options.

## Usage

``` r
DimPlot(
  data,
  dims = 1:2,
  group_by = NULL,
  group_by_sep = "_",
  split_by = NULL,
  split_by_sep = "_",
  pt_size = NULL,
  pt_alpha = 1,
  bg_color = "grey80",
  label = FALSE,
  label_insitu = FALSE,
  label_size = 4,
  label_fg = "white",
  label_bg = "black",
  label_bg_r = 0.1,
  label_repel = FALSE,
  label_repulsion = 20,
  label_pt_size = 1,
  label_pt_color = "black",
  label_segment_color = "black",
  show_stat = !identical(theme, "theme_blank"),
  order = c("as-is", "reverse", "high-top", "low-top", "random"),
  highlight = NULL,
  highlight_alpha = 1,
  highlight_size = 1,
  highlight_color = "black",
  highlight_stroke = 0.8,
  add_mark = FALSE,
  mark_type = c("hull", "ellipse", "rect", "circle"),
  mark_expand = grid::unit(3, "mm"),
  mark_alpha = 0.1,
  mark_linetype = 1,
  add_density = FALSE,
  density_color = "grey80",
  density_filled = FALSE,
  density_filled_palette = "Greys",
  density_filled_palcolor = NULL,
  raster = NULL,
  raster_dpi = c(512, 512),
  hex = FALSE,
  hex_linewidth = 0.5,
  hex_count = TRUE,
  hex_bins = 50,
  hex_binwidth = NULL,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  theme = "theme_ggforge_grid",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  keep_empty = FALSE,
  seed = 8525,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  axes = NULL,
  axis_titles = NULL,
  guides = NULL,
  design = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- dims:

  Column names or indices for x and y axes. Default is first two
  columns.

- group_by:

  Column name for categorical grouping. Creates discrete color mapping.
  If multiple columns provided, they will be concatenated with
  `group_by_sep`.

- group_by_sep:

  Separator for concatenating multiple `group_by` columns.

- split_by:

  Column name(s) to split data into multiple plots

- split_by_sep:

  Separator when concatenating multiple split_by columns

- pt_size:

  Point size. If NULL, calculated based on number of data points.

- pt_alpha:

  Point transparency (0-1).

- bg_color:

  Color for NA/background points.

- label:

  Whether to show group labels at median positions.

- label_insitu:

  Whether to use actual group names as labels (vs. numbers).

- label_size:

  Size of labels.

- label_fg, label_bg:

  Foreground and background colors for labels.

- label_bg_r:

  Background radius for labels.

- label_repel:

  Whether to repel overlapping labels.

- label_repulsion:

  Repulsion force for labels.

- label_pt_size, label_pt_color:

  Point size and color for label anchors.

- label_segment_color:

  Color of segments connecting labels to points.

- show_stat:

  Whether to show sample count in subtitle.

- order:

  How to order points: "as-is", "reverse", "high-top", "low-top",
  "random". Affects drawing order (what's on top).

- highlight:

  Row names/indices to highlight, or logical expression as string.

- highlight_alpha, highlight_size, highlight_color, highlight_stroke:

  Styling for highlighted points.

- add_mark:

  Whether to add marks (hulls/ellipses) around groups.

- mark_type:

  Type of mark: "hull", "ellipse", "rect", "circle".

- mark_expand, mark_alpha, mark_linetype:

  Mark styling parameters.

- add_density:

  Whether to add 2D density contours/fill.

- density_color, density_filled:

  Density styling.

- density_filled_palette, density_filled_palcolor:

  Palette for filled density.

- raster:

  Whether to rasterize points (useful for large datasets).

- raster_dpi:

  DPI for rasterization.

- hex:

  Whether to use hexagonal binning instead of points.

- hex_linewidth, hex_count, hex_bins, hex_binwidth:

  Hex bin parameters.

- facet_by:

  Column name(s) for faceting the plot

- facet_scales:

  Scales for facets: "fixed", "free", "free_x", "free_y"

- facet_nrow:

  Number of rows in facet layout

- facet_ncol:

  Number of columns in facet layout

- facet_byrow:

  Fill facets by row (TRUE) or column (FALSE)

- theme:

  Theme name (string) or theme function

- theme_args:

  List of arguments passed to theme function

- palette:

  Color palette name

- palcolor:

  Custom colors for palette

- alpha:

  Transparency level (0-1)

- aspect.ratio:

  Aspect ratio of plot panel

- legend.position:

  Legend position: "none", "left", "right", "bottom", "top"

- legend.direction:

  Legend direction: "horizontal" or "vertical"

- title:

  Plot title

- subtitle:

  Plot subtitle

- xlab:

  X-axis label

- ylab:

  Y-axis label

- keep_empty:

  Keep empty factor levels

- seed:

  Random seed for reproducibility

- combine:

  Whether to combine split plots into one

- nrow:

  Number of rows when combining plots

- ncol:

  Number of columns when combining plots

- byrow:

  Fill combined plots by row

- axes:

  How to handle axes in combined plots ("keep", "collect", "collect_x",
  "collect_y")

- axis_titles:

  How to handle axis titles in combined plots

- guides:

  How to handle guides in combined plots ("collect", "keep", "auto")

- design:

  Custom layout design for combined plots

## Value

A ggplot object or list/combined plots

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
DimPlot(data, dims = c("UMAP_1", "UMAP_2"), group_by = "clusters")

# With faceting
DimPlot(data, dims = 1:2, group_by = "clusters", facet_by = "condition")

# With splitting
DimPlot(data, dims = 1:2, group_by = "clusters", split_by = "timepoint")
} # }
```
