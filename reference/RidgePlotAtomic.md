# Ridge Plot Atomic Function

Internal function to create ridge plots without splitting

## Usage

``` r
RidgePlotAtomic(
  data,
  x = NULL,
  in_form = c("long", "wide"),
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  add_vline = NULL,
  vline_type = "solid",
  vline_color = TRUE,
  vline_width = 0.5,
  vline_alpha = 1,
  flip = FALSE,
  alpha = 0.8,
  scale = NULL,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  x_text_angle = 90,
  keep_empty = FALSE,
  reverse = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
  legend.position = "none",
  legend.direction = "vertical",
  ...
)
```

## Arguments

- data:

  A data frame (long or wide form)

- x:

  Column name for values (numeric expected)

- in_form:

  Data format: "long" or "wide"

- group_by:

  Column(s) to group the data (shown on y-axis)

- group_by_sep:

  Separator for concatenating multiple group_by columns

- group_name:

  Legend title for group_by

- add_vline:

  Add vertical lines (TRUE for mean, numeric vector, or named list)

- vline_type:

  Line type for vertical lines

- vline_color:

  Color for vertical lines (TRUE to match groups)

- vline_width:

  Width of vertical lines

- vline_alpha:

  Alpha for vertical lines

- flip:

  Whether to flip the plot

- alpha:

  Transparency for ridges

- scale:

  Scaling factor for ridges

- theme:

  Theme function

- theme_args:

  Arguments for theme function

- palette:

  Palette name

- palcolor:

  Custom colors

- title:

  Plot title

- subtitle:

  Plot subtitle

- xlab:

  X-axis label

- ylab:

  Y-axis label

- x_text_angle:

  Angle for x-axis text

- keep_empty:

  Keep empty groups on y-axis

- reverse:

  Reverse order of groups on y-axis

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

- aspect.ratio:

  Aspect ratio

- legend.position:

  Legend position

- legend.direction:

  Legend direction

- ...:

  Additional arguments passed to geom_density_ridges

## Value

A ggplot object
