# Box/Violin Plot

Create box or violin plots with optional grouping, faceting, and
statistical comparisons. This is the main wrapper function that handles
data splitting and combines multiple plots.

## Usage

``` r
BoxViolinPlot(
  data,
  x,
  x_sep = "_",
  y = NULL,
  base = c("box", "violin"),
  in_form = c("long", "wide"),
  split_by = NULL,
  split_by_sep = "_",
  symnum_args = NULL,
  sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc",
    "median"),
  flip = FALSE,
  keep_empty = FALSE,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  paired_by = NULL,
  x_text_angle = NULL,
  step_increase = 0.1,
  fill_mode = ifelse(!is.null(group_by), "dodge", "x"),
  fill_reverse = FALSE,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  aspect.ratio = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  add_point = FALSE,
  pt_color = "grey30",
  pt_size = NULL,
  pt_alpha = 1,
  jitter_width = NULL,
  jitter_height = 0,
  stack = FALSE,
  y_max = NULL,
  y_min = NULL,
  add_box = FALSE,
  box_color = "black",
  box_width = 0.1,
  box_ptsize = 2.5,
  add_trend = FALSE,
  trend_color = NULL,
  trend_linewidth = 1,
  trend_ptsize = 2,
  add_stat = NULL,
  stat_name = NULL,
  stat_color = "black",
  stat_size = 1,
  stat_stroke = 1,
  stat_shape = 25,
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  add_line = NULL,
  line_color = "red2",
  line_width = 0.6,
  line_type = 2,
  highlight = NULL,
  highlight_color = "red2",
  highlight_size = 1,
  highlight_alpha = 1,
  comparisons = NULL,
  ref_group = NULL,
  pairwise_method = "wilcox.test",
  multiplegroup_comparisons = FALSE,
  multiple_method = "kruskal.test",
  sig_label = "p.format",
  sig_labelsize = 3.5,
  hide_ns = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  axes = NULL,
  axis_titles = axes,
  guides = NULL,
  design = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- x:

  Column for x-axis (discrete). Can be a single column name or multiple
  columns that will be concatenated.

- y:

  Column for y-axis (numeric). The response variable.

- base:

  Plot base type: "box" or "violin"

- in_form:

  Input data form: "long" (default) or "wide"

- split_by:

  Column name(s) to split data into multiple plots

- split_by_sep:

  Separator when concatenating multiple split_by columns

- symnum_args:

  Symbolic number coding arguments for significance

- sort_x:

  Sort x-axis values: "none", "mean_asc", "mean_desc", "mean",
  "median_asc", "median_desc", "median"

- flip:

  Logical; flip coordinates to create horizontal plots

- keep_empty:

  Logical; keep empty factor levels on x-axis

- group_by:

  Column for grouping (creates dodged/side-by-side plots)

- group_by_sep:

  Separator when concatenating multiple group_by columns

- group_name:

  Legend name for groups

- paired_by:

  Column identifying paired observations (for paired tests)

- x_text_angle:

  Angle for x-axis text labels

- step_increase:

  Step increase for comparison brackets

- fill_mode:

  Fill coloring mode: "dodge", "x", "mean", or "median"

- fill_reverse:

  Logical; reverse gradient fills

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

- add_point:

  Logical; add jittered data points

- pt_color:

  Point color (default: "grey30")

- pt_size:

  Point size (auto-calculated if NULL)

- pt_alpha:

  Point transparency (0-1)

- jitter_width:

  Jitter width for points

- jitter_height:

  Jitter height for points

- stack:

  Logical; stack facets vertically/horizontally

- y_max:

  Y-axis maximum (numeric or "qXX" for quantile)

- y_min:

  Y-axis minimum (numeric or "qXX" for quantile)

- add_box:

  Logical; add box overlay (violin only)

- box_color:

  Box overlay color

- box_width:

  Box overlay width

- box_ptsize:

  Box median point size

- add_trend:

  Logical; add trend line connecting medians

- trend_color:

  Trend line color

- trend_linewidth:

  Trend line width

- trend_ptsize:

  Trend point size

- add_stat:

  Function to add stat summary (e.g., mean)

- stat_name:

  Stat legend name

- stat_color:

  Stat point color

- stat_size:

  Stat point size

- stat_stroke:

  Stat point stroke width

- stat_shape:

  Stat point shape

- add_bg:

  Logical; add alternating background

- bg_palette:

  Background color palette

- bg_palcolor:

  Background custom colors

- bg_alpha:

  Background transparency

- add_line:

  Numeric; add horizontal reference line at this value

- line_color:

  Reference line color

- line_width:

  Reference line width

- line_type:

  Reference line type

- highlight:

  Points to highlight (logical, indices, or expression)

- highlight_color:

  Highlight color

- highlight_size:

  Highlight size

- highlight_alpha:

  Highlight transparency

- comparisons:

  Pairwise comparisons (list of pairs or TRUE for all)

- ref_group:

  Reference group for comparisons

- pairwise_method:

  Statistical method for pairwise comparisons

- multiplegroup_comparisons:

  Logical; perform multiple group comparisons

- multiple_method:

  Statistical method for multiple comparisons

- sig_label:

  Significance label format: "p.format" or "p.signif"

- sig_labelsize:

  Significance label font size

- hide_ns:

  Logical; hide non-significant comparisons

- facet_by:

  Column name(s) for faceting the plot

- facet_scales:

  Scales for facets: "fixed", "free", "free_x", "free_y"

- facet_ncol:

  Number of columns in facet layout

- facet_nrow:

  Number of rows in facet layout

- facet_byrow:

  Fill facets by row (TRUE) or column (FALSE)

- title:

  Plot title

- subtitle:

  Plot subtitle

- xlab:

  X-axis label

- ylab:

  Y-axis label

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

- y_trans:

  Y-axis transformation (e.g., "log10", "sqrt")

- y_nbreaks:

  Number of y-axis breaks

## Value

A ggplot object or combined plots (patchwork)
