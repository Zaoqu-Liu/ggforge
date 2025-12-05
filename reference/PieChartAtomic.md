# Pie Chart (Atomic)

Creates a single pie chart without data splitting

## Usage

``` r
PieChartAtomic(
  data,
  x,
  y = NULL,
  label = y,
  clockwise = TRUE,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  facet_by = NULL,
  facet_scales = "free_y",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
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

  Column name for the x-axis (categories). Will be converted to factor.

- y:

  Column name for the y-axis (values). If NULL, counts will be used.

- label:

  Column to use for labels. NULL means no label. Default is the same
  as y. If y is NULL, use ".y" to specify the count as the label.

- clockwise:

  Logical value to draw the pie chart clockwise or counter-clockwise.

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

## Value

A ggplot object
