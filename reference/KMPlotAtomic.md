# Kaplan-Meier Survival Plot Atomic

Creates a single Kaplan-Meier survival plot without splitting. This is
the core plotting function that handles the actual ggplot construction.

## Usage

``` r
KMPlotAtomic(
  data,
  time,
  status,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  show_pval = TRUE,
  pval_method = "logrank",
  pval_digits = 4,
  pval_size = 4.5,
  pval_coord = c(0.05, 0.1),
  show_conf_int = FALSE,
  conf_alpha = 0.2,
  show_median_line = "none",
  median_linetype = 2,
  median_linewidth = 0.6,
  line_width = 1.3,
  show_risk_table = FALSE,
  risk_table_height = 0.25,
  risk_table_fontsize = 3.5,
  show_censors = TRUE,
  censor_shape = 3,
  censor_size = 4,
  censor_stroke = 0.5,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  aspect.ratio = NULL,
  x_breaks = NULL,
  y_breaks = waiver(),
  x_min = NULL,
  x_max = NULL,
  y_min = 0,
  y_max = 1,
  legend.position = "top",
  legend.direction = "horizontal",
  title = NULL,
  subtitle = NULL,
  xlab = "Time",
  ylab = "Survival Probability",
  facet_by = NULL,
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- group_by:

  Column name(s) for grouping data

- group_by_sep:

  Separator when concatenating multiple group_by columns

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
