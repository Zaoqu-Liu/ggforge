# Density Plot

Creates density plots to illustrate the distribution of continuous data.
Supports grouping, faceting, and splitting into multiple plots.

## Usage

``` r
DensityPlot(
  data,
  x,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  xtrans = "identity",
  ytrans = "identity",
  split_by = NULL,
  split_by_sep = "_",
  flip = FALSE,
  position = "identity",
  palette = "Paired",
  palcolor = NULL,
  alpha = 0.5,
  theme = "theme_ggforge",
  theme_args = list(),
  add_bars = FALSE,
  bar_height = 0.025,
  bar_alpha = 1,
  bar_width = 0.1,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  expand = c(bottom = 0, left = 0, right = 0),
  facet_by = NULL,
  facet_scales = "free_y",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  legend.position = waiver(),
  legend.direction = "vertical",
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

- split_by:

  Column to split data into multiple plots

- split_by_sep:

  Separator for concatenating multiple split_by columns

- flip:

  Whether to flip the plot

- position:

  Position adjustment for overlapping groups

- palette:

  Palette name

- palcolor:

  Custom colors

- alpha:

  Transparency for density curves

- theme:

  Theme name or function

- theme_args:

  Arguments passed to theme function

- add_bars:

  Whether to add data distribution lines at bottom

- bar_height:

  Height of distribution bars (as fraction of max)

- bar_alpha:

  Alpha for distribution bars

- bar_width:

  Width of distribution bars

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

- seed:

  Random seed

- combine:

  Whether to combine multiple plots

- nrow:

  Number of rows for combined plots

- ncol:

  Number of columns for combined plots

- byrow:

  Fill combined plots by row

- axes:

  Axis handling for combined plots

- axis_titles:

  Axis title handling for combined plots

- guides:

  Guide handling for combined plots

- design:

  Custom design for combined plots

- ...:

  Additional arguments passed to geom_density

## Value

A ggplot object, combined plot, or list of plots

## Examples

``` r
set.seed(8525)
data <- data.frame(
  x = c(rnorm(500, -1), rnorm(500, 1)),
  group = rep(c("A", "B"), each = 500),
  facet = sample(c("F1", "F2"), 1000, replace = TRUE)
)

DensityPlot(data, x = "x")

DensityPlot(data, x = "x", group_by = "group")

DensityPlot(data, x = "x", group_by = "group", facet_by = "facet")

DensityPlot(data, x = "x", split_by = "facet", add_bars = TRUE)
```
