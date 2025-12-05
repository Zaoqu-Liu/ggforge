# QQ Plot

QQ plot is a graphical tool to compare two distributions by plotting
their quantiles against each other. Can also create PP
(probability-probability) plots.

## Usage

``` r
QQPlot(
  data,
  val,
  val_trans = NULL,
  type = c("qq", "pp"),
  band = NULL,
  line = list(),
  point = list(),
  fill_name = "Bands",
  band_alpha = 0.5,
  split_by = NULL,
  split_by_sep = "_",
  facet_by = NULL,
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
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
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  axes = NULL,
  axis_titles = NULL,
  guides = NULL,
  design = NULL,
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

- split_by:

  Column name(s) to split data into multiple plots

- split_by_sep:

  Separator when concatenating multiple split_by columns

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

- theme:

  Theme name (string) or theme function

- theme_args:

  List of arguments passed to theme function

- palette:

  Color palette name

- palcolor:

  Custom colors for palette

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

- combine:

  Whether to combine split plots into one

- nrow:

  Number of rows when combining plots

- ncol:

  Number of columns when combining plots

- byrow:

  Fill combined plots by row

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

A ggplot object or wrap_plots object or a list of ggplot objects

## Examples

``` r
set.seed(8525)
data <- data.frame(norm = rnorm(100))

QQPlot(data, val = "norm", band = TRUE)

QQPlot(data, val = "norm", band = list(
  list(bandType = "ks", mapping = ggplot2::aes(fill = "KS"), alpha = 0.3),
  list(bandType = "ts", mapping = ggplot2::aes(fill = "TS")),
  list(bandType = "pointwise", mapping = ggplot2::aes(fill = "Normal")),
  list(bandType = "boot", mapping = ggplot2::aes(fill = "Bootstrap"))
), band_alpha = 0.6)


if (FALSE) { # \dontrun{
data(airquality, package = "datasets")
di <- "exp" # exponential distribution
dp <- list(rate = 2) # exponential rate parameter
QQPlot(airquality,
  val = "Ozone",
  band = list(distribution = di, dparams = dp),
  line = list(distribution = di, dparams = dp),
  point = list(distribution = di, dparams = dp)
)

de <- TRUE # enabling the detrend option
QQPlot(airquality,
  val = "Ozone",
  band = list(distribution = di, dparams = dp, detrend = de),
  line = list(distribution = di, dparams = dp, detrend = de),
  point = list(distribution = di, dparams = dp, detrend = de)
)

QQPlot(data, val = "norm", type = "pp", band = TRUE)

dp <- list(mean = 2, sd = 2) # shifted and rescaled Normal parameters
QQPlot(data,
  val = "norm", type = "pp",
  band = list(dparams = dp),
  point = list(dparams = dp)
)

QQPlot(data,
  val = "norm", type = "pp", band = TRUE,
  line = list(ab = c(.2, .5))
)

di <- "exp"
dp <- list(rate = .022) # value is based on some empirical tests
de <- TRUE
QQPlot(airquality,
  val = "Ozone", type = "pp",
  band = list(distribution = di, detrend = de, dparams = dp),
  line = list(detrend = de),
  point = list(distribution = di, detrend = de, dparams = dp),
  ylim = c(-.5, .5)
)
} # }
```
