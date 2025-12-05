# QQ Plot Atomic

Creates a single QQ or PP plot without splitting

## Usage

``` r
QQPlotAtomic(
  data,
  val,
  val_trans = NULL,
  type = "qq",
  band = NULL,
  line = list(),
  point = list(),
  fill_name = "Bands",
  band_alpha = 0.5,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
  legend.position = waiver(),
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlim = NULL,
  ylim = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525,
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- val:

  A character string of the column name for the values to plot. A
  numeric column is expected.

- val_trans:

  A function to transform the values before plotting. Default is NULL,
  which means no transformation.

- type:

  A character string to specify the type of plot. Default is "qq", which
  means QQ plot. Other option is "pp" for PP plot.

- band:

  A list of arguments to pass to
  [`qqplotr::stat_qq_band()`](https://rdrr.io/pkg/qqplotr/man/stat_qq_band.html)
  or
  [`qqplotr::stat_pp_band()`](https://rdrr.io/pkg/qqplotr/man/stat_pp_band.html),
  depending on the value of `type`. Default is NULL, which means no
  band. If an empty list or TRUE is provided, the default arguments will
  be used. Multiple bands can be added by providing a list of lists.

- line:

  A list of arguments to pass to `qqplotr::stat_qq_line()` or
  [`qqplotr::stat_pp_line()`](https://rdrr.io/pkg/qqplotr/man/stat_pp_line.html),
  depending on the value of `type`. Default is
  [`list()`](https://rdrr.io/r/base/list.html), which means to add a
  line with default arguments. If `NULL` is provided, no line will be
  added.

- point:

  A list of arguments to pass to
  [`qqplotr::stat_qq_point()`](https://rdrr.io/pkg/qqplotr/man/stat_qq_point.html)
  or
  [`qqplotr::stat_pp_point()`](https://rdrr.io/pkg/qqplotr/man/stat_pp_point.html),
  depending on the value of `type`. Default is
  [`list()`](https://rdrr.io/r/base/list.html), which means to add
  points with default arguments. If `NULL` is provided, no points will
  be added (not recommended).

- fill_name:

  A character string to name the legend of fill. Default is "Bands".

- band_alpha:

  A numeric value to set the alpha of all bands. Default is 0.5. It is a
  shortcut for setting alpha of all bands. You can override it by
  setting `alpha` in `band` argument.

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

- xlim:

  A numeric vector of length 2 to set the x-axis limits.

- ylim:

  A numeric vector of length 2 to set the y-axis limits.

- xlab:

  X-axis label

- ylab:

  Y-axis label

- seed:

  Random seed for reproducibility
