# Scatter Plot (Atomic)

Creates a single scatter plot without data splitting

## Usage

``` r
ScatterPlotAtomic(
  data,
  x,
  y,
  size_by = 2,
  size_name = NULL,
  color_by = NULL,
  color_name = NULL,
  color_reverse = FALSE,
  highlight = NULL,
  highlight_shape = 16,
  highlight_size = 3,
  highlight_color = "red",
  highlight_alpha = 1,
  theme = "theme_ggforge",
  theme_args = list(),
  alpha = NULL,
  shape = 21,
  border_color = "black",
  xtrans = "identity",
  ytrans = "identity",
  palette = NULL,
  palcolor = NULL,
  facet_by = NULL,
  facet_scales = "fixed",
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
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- x:

  Column name for x-axis. Must be a numeric column.

- y:

  Column name for y-axis. Must be a numeric column.

- size_by:

  Column name to use for point size, or a numeric value for fixed size.

- size_name:

  Name for the size legend.

- color_by:

  Column name to use for point color. Can be numeric or categorical.

- color_name:

  Name for the color legend.

- color_reverse:

  Whether to reverse the color direction.

- highlight:

  Vector of row indices/names or expression string to highlight points.

- highlight_shape:

  Shape for highlighted points.

- highlight_size:

  Size for highlighted points.

- highlight_color:

  Color for highlighted points.

- highlight_alpha:

  Alpha for highlighted points.

- theme:

  Theme name (string) or theme function

- theme_args:

  List of arguments passed to theme function

- alpha:

  Transparency level (0-1)

- shape:

  Shape of points (default 21 which has fill).

- border_color:

  Color for point borders, or TRUE to match fill color.

- xtrans:

  Transformation for x-axis (e.g. "log10", "sqrt").

- ytrans:

  Transformation for y-axis (e.g. "log10", "sqrt").

- palette:

  Color palette name

- palcolor:

  Custom colors for palette

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

## Value

A ggplot object
