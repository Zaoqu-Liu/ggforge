# Cox Plot Atomic Function

Creates a single Cox plot without splitting

## Usage

``` r
CoxPlotAtomic(
  data,
  time,
  event,
  vars,
  var,
  plot_type,
  scale,
  nonExpression_ratio,
  parallel,
  n_cores,
  ribbon_color,
  ribbon_alpha,
  line_color,
  line_type,
  line_width,
  show_cindex,
  text_size,
  text_digit,
  text_face,
  point_colors,
  point_size,
  point_border_width,
  point_border_size,
  line_colors,
  cutoff_vline_type,
  cutoff_vline_width,
  cutoff_vline_color,
  x_log10_scale,
  text_colors,
  digits,
  rel_width,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  aspect.ratio = NULL,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  seed = 8525,
  ...
)
```

## Arguments

- data:

  A data frame containing survival time, event status, and variables for
  Cox analysis.

- time:

  Column name for time variable (numeric).

- event:

  Column name for event status (1=event, 0=censored).

- vars:

  Column name(s) for variables to include in Cox regression. If NULL,
  uses `var`.

- var:

  Column name for single variable (used for curve plot or when vars is
  NULL).

- plot_type:

  Type of Cox plot: "curve" (hazard ratio curve), "forest" (simple
  forest plot), or "forest2" (detailed forest plot with HR and p-value
  columns).

- scale:

  Logical. Whether to standardize variables to z-scores before Cox
  regression.

- nonExpression_ratio:

  Numeric. Threshold ratio for filtering non-expressed genes. If the
  proportion of zero values exceeds this ratio, the variable will be
  filtered out.

- parallel:

  Logical. Whether to perform parallel computation for multiple
  variables.

- n_cores:

  Integer. Number of cores to use for parallel computation. Default is
  detectCores() - 6, minimum 1.

- ribbon_color:

  Color for confidence interval ribbon (curve plot).

- ribbon_alpha:

  Alpha transparency for ribbon (curve plot).

- line_color:

  Color for hazard ratio line (curve plot).

- line_type:

  Line type for hazard ratio curve.

- line_width:

  Line width for hazard ratio curve.

- show_cindex:

  Logical. Whether to show concordance index (Cindex) on plot.

- text_size:

  Size of annotation text.

- text_digit:

  Number of significant digits for text display.

- text_face:

  Font face for annotation text.

- point_colors:

  Vector of colors for points in forest plot. Default: c("#ED6355",
  "#118ab2", "grey") for Risky, Protective, NoSig.

- point_size:

  Size of points in forest plot.

- point_border_width:

  Border width of points.

- point_border_size:

  Size of point borders.

- line_colors:

  Colors for error bars in forest plot.

- cutoff_vline_type:

  Line type for cutoff vertical line (HR = 1).

- cutoff_vline_width:

  Line width for cutoff vertical line.

- cutoff_vline_color:

  Color for cutoff vertical line.

- x_log10_scale:

  Logical. Whether to use log10 scale for x-axis in forest plot.

- text_colors:

  Text colors in forest2 plot.

- digits:

  Number of significant digits for numeric display.

- rel_width:

  Relative widths of columns in forest2 plot. Default: c(0.8, 1.4, 1.2,
  0.6) for Variable, HR plot, HR text, P-value.

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

- title:

  Plot title

- subtitle:

  Plot subtitle

- xlab:

  X-axis label

- ylab:

  Y-axis label

- legend.position:

  Legend position: "none", "left", "right", "bottom", "top"

- legend.direction:

  Legend direction: "horizontal" or "vertical"

- seed:

  Random seed for reproducibility
