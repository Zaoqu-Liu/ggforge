# Trend Plot

Creates trend plots that combine area and bar visualizations to show
trends. Like an area plot but with gaps between the bars. Supports
splitting by groups, faceting, and custom color palettes.

## Usage

``` r
TrendPlot(
  data,
  x,
  y = NULL,
  x_sep = "_",
  split_by = NULL,
  split_by_sep = "_",
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
  legend.position = ggplot2::waiver(),
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  seed = 8525,
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

  Column name for x-axis variable

- y:

  Column name for y-axis variable

- x_sep:

  Separator for concatenating multiple x columns.

- split_by:

  Column name(s) to split data into multiple plots

- split_by_sep:

  Separator when concatenating multiple split_by columns

- group_by:

  Column name(s) for grouping data

- group_by_sep:

  Separator when concatenating multiple group_by columns

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

- combine:

  Whether to combine split plots into one

- nrow:

  Number of rows when combining plots

- ncol:

  Number of columns when combining plots

- byrow:

  Fill combined plots by row

- seed:

  Random seed for reproducibility

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

A ggplot object, patchwork object (if combine=TRUE), or list of plots

## See also

[`AreaPlot`](https://zaoqu-liu.github.io/ggforge/reference/AreaPlot.md)

## Examples

``` r
# Create sample data
data <- data.frame(
  x = rep(c("A", "B", "C", "D"), 2),
  y = c(1, 3, 6, 4, 2, 5, 7, 8),
  group = rep(c("F1", "F2"), each = 4)
)

# Basic trend plot
TrendPlot(data, x = "x", y = "y", group_by = "group")


# With scaled y-axis
TrendPlot(data, x = "x", y = "y", group_by = "group", scale_y = TRUE)


# Split by group
TrendPlot(data, x = "x", y = "y", split_by = "group")


# Custom palettes per split
TrendPlot(data,
  x = "x", y = "y", split_by = "group",
  palette = list(F1 = "Set1", F2 = "Paired")
)
```
