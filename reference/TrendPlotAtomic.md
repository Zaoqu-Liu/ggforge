# Trend Plot (Atomic)

Creates a single trend plot without data splitting. A trend plot is like
an area plot but with bars showing the trend.

## Usage

``` r
TrendPlotAtomic(
  data,
  x,
  y = NULL,
  x_sep = "_",
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  scale_y = FALSE,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  x_text_angle = 0,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- x:

  Column name for x-axis (categories). Will be converted to factor.

- y:

  Column name for y-axis (values). If NULL, counts will be used.

- x_sep:

  Separator for concatenating multiple x columns.

- group_by:

  Column(s) to group/stack by.

- group_by_sep:

  Separator for concatenating multiple group_by columns.

- group_name:

  Name for the group legend.

- scale_y:

  Whether to scale y-axis to proportions (0-1).

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

- x_text_angle:

  Angle for x-axis text.

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

## Value

A ggplot object
