# Area Plot Atomic

Creates a single area plot without splitting

## Usage

``` r
AreaPlotAtomic(
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
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  x_text_angle = 0,
  aspect.ratio = 1,
  legend.position = waiver(),
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

  A character string of the column name to plot on the x-axis. A
  character/factor column is expected.

- y:

  A character string of the column name to plot on the y-axis. A numeric
  column is expected. If NULL, the count of the x-axis column will be
  used.

- x_sep:

  A character string to concatenate the columns in `x`, if multiple
  columns are provided.

- group_by:

  A character vector of column names to fill the area plot by. If NULL,
  the plot will be filled by the first color of the palette. If multiple
  columns are provided, the columns will be concatenated with
  `group_by_sep` and used as the fill column.

- group_by_sep:

  A character string to separate the columns in `group_by`.

- group_name:

  A character string to name the legend of fill.

- scale_y:

  A logical value to scale the y-axis by the total number in each x-axis
  group.

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

- facet_nrow:

  Number of rows in facet layout

- facet_ncol:

  Number of columns in facet layout

- facet_byrow:

  Fill facets by row (TRUE) or column (FALSE)

- x_text_angle:

  Angle for x-axis text

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

  Whether to keep empty factor levels
