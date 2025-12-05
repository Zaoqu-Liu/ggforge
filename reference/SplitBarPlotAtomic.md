# Split Bar Plot Atomic (Waterfall Plot)

Creates a split bar plot showing positive and negative values on
opposite sides. Also known as a waterfall plot or diverging bar plot.

## Usage

``` r
SplitBarPlotAtomic(
  data,
  x,
  y,
  y_sep = "_",
  flip = FALSE,
  alpha_by = NULL,
  alpha_reverse = FALSE,
  alpha_name = NULL,
  order_y = list(`+` = c("x_desc", "alpha_desc"), `-` = c("x_desc", "alpha_asc")),
  bar_height = 0.9,
  lineheight = 0.5,
  max_charwidth = 80,
  fill_by = NULL,
  fill_by_sep = "_",
  fill_name = NULL,
  direction_pos_name = "positive",
  direction_neg_name = "negative",
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  facet_by = NULL,
  facet_scales = "free_y",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
  x_min = NULL,
  x_max = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  keep_empty = FALSE,
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

- theme:

  Theme name (string) or theme function

- theme_args:

  List of arguments passed to theme function

- palette:

  Color palette name

- palcolor:

  Custom colors for palette

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
