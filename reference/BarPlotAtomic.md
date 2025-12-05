# Bar Plot Atomic

Creates a single bar plot without splitting. Handles both grouped and
ungrouped bar plots.

## Usage

``` r
BarPlotAtomic(
  data,
  x,
  x_sep = "_",
  y = NULL,
  scale_y = FALSE,
  flip = FALSE,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  fill_by_x_if_no_group = TRUE,
  label = NULL,
  label_nudge = 0.02,
  label_fg = "black",
  label_size = 4,
  label_bg = "white",
  label_bg_r = 0.1,
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  x_text_angle = 0,
  aspect.ratio = 1,
  add_line = NULL,
  line_color = "red2",
  line_width = 0.6,
  line_type = 2,
  line_name = NULL,
  add_trend = FALSE,
  trend_color = "black",
  trend_linewidth = 1,
  trend_ptsize = 2.5,
  position = "auto",
  position_dodge_preserve = "total",
  y_min = NULL,
  y_max = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  keep_empty = FALSE,
  expand = waiver(),
  width = waiver(),
  facet_by = NULL,
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- x:

  Column name for x-axis variable

- y:

  Column name for y-axis variable

- group_by:

  Column name(s) for grouping data

- group_by_sep:

  Separator when concatenating multiple group_by columns

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

- x_text_angle:

  Angle for x-axis text labels

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

- expand:

  Expansion values for plot axes (CSS-like padding)

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
