# Density Plot Atomic Function

Internal function to create density or histogram plots without splitting

## Usage

``` r
DensityHistoPlotAtomic(
  data,
  x,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  xtrans = "identity",
  ytrans = "identity",
  type = c("density", "histogram"),
  bins = NULL,
  binwidth = NULL,
  flip = FALSE,
  add_bars = FALSE,
  bar_height = 0.025,
  bar_alpha = 1,
  bar_width = 0.1,
  position = "identity",
  use_trend = FALSE,
  add_trend = FALSE,
  trend_alpha = 1,
  trend_linewidth = 0.8,
  trend_pt_size = 1.5,
  trend_skip_zero = FALSE,
  palette = "Paired",
  palcolor = NULL,
  alpha = 0.5,
  theme = "theme_ggforge",
  theme_args = list(),
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  expand = c(bottom = 0, left = 0, right = 0),
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  legend.position = "right",
  legend.direction = "vertical",
  ...
)
```

## Arguments

- data:

  A data frame

- x:

  Column name for values (numeric expected)

- group_by:

  Column to group the data

- group_by_sep:

  Separator for concatenating multiple group_by columns

- group_name:

  Legend title for group_by

- xtrans:

  Transformation for x-axis (e.g., "identity", "log10")

- ytrans:

  Transformation for y-axis

- type:

  Plot type: "density" or "histogram"

- bins:

  Number of bins for histogram

- binwidth:

  Width of bins for histogram

- flip:

  Whether to flip the plot

- add_bars:

  Whether to add data distribution lines at bottom

- bar_height:

  Height of distribution bars (as fraction of max)

- bar_alpha:

  Alpha for distribution bars

- bar_width:

  Width of distribution bars

- position:

  Position adjustment for overlapping groups

- use_trend:

  Use trend line instead of histogram bars

- add_trend:

  Add trend line to histogram

- trend_alpha:

  Alpha for trend line and points

- trend_linewidth:

  Width of trend line

- trend_pt_size:

  Size of trend points

- trend_skip_zero:

  Skip zero counts in trend line

- palette:

  Palette name

- palcolor:

  Custom colors

- alpha:

  Transparency for density/histogram

- theme:

  Theme function

- theme_args:

  Arguments for theme function

- title:

  Plot title

- subtitle:

  Plot subtitle

- xlab:

  X-axis label

- ylab:

  Y-axis label

- expand:

  Axis expansion

- facet_by:

  Columns for faceting

- facet_scales:

  Facet scales type

- facet_ncol:

  Number of facet columns

- facet_nrow:

  Number of facet rows

- facet_byrow:

  Fill facets by row

- legend.position:

  Legend position

- legend.direction:

  Legend direction

- ...:

  Additional arguments passed to geoms

## Value

A ggplot object
