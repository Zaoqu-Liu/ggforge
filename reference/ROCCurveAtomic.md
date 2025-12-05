# ROC Curve Plot Atomic

Creates a single ROC curve plot without splitting

## Usage

``` r
ROCCurveAtomic(
  data,
  truth_by,
  score_by,
  pos_label = NULL,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  x_axis_reverse = FALSE,
  percent = FALSE,
  ci = NULL,
  n_cuts = 0,
  cutoffs_at = NULL,
  cutoffs_labels = NULL,
  cutoffs_accuracy = 0.01,
  cutoffs_pt_size = 5,
  cutoffs_pt_shape = 4,
  cutoffs_pt_stroke = 1,
  cutoffs_labal_fg = "black",
  cutoffs_label_size = 4,
  cutoffs_label_bg = "white",
  cutoffs_label_bg_r = 0.1,
  show_auc = c("auto", "none", "legend", "plot"),
  auc_accuracy = 0.01,
  auc_size = 4,
  increasing = TRUE,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  alpha = 1,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
  legend.position = waiver(),
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = ifelse(x_axis_reverse, "Specificity", "1 - Specificity"),
  ylab = "Sensitivity",
  ...
)
```

## Arguments

- data:

  A data frame with the truth and score columns. See also
  https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html.

- truth_by:

  A character string of the column name that contains the true class
  labels. (a.k.a. the binary outcome, 1/0 or TRUE/FALSE.)

- score_by:

  character strings of the column names that contains the predicted
  scores. When multiple columns are provided, the ROC curve is plotted
  for each column.

- pos_label:

  A character string of the positive class label. When NULL, the labels
  will be handled by the `plotROC` package.

- group_by:

  A character vector of column names to group the ROC curve by. When
  `score_by` contains multiple columns, `group_by` should be NULL.

- group_by_sep:

  A character string to separate the columns in `group_by`.

- group_name:

  A character string to name the legend of the ROC curve groups.

- x_axis_reverse:

  A logical to reverse the x-axis, that is from 1 to 0.

- percent:

  A logical to display the x and y axis as percentages.

- ci:

  A list of arguments to pass to
  [`plotROC::geom_rocci()`](https://sachsmc.github.io/plotROC/reference/geom_rocci.html)
  to add confidence intervals. When NULL, no confidence intervals are
  added.

- n_cuts:

  An integer to specify the number of cutpoints on the ROC curve. It
  will be the quantiles of the predicted scores.

- cutoffs_at:

  Vector of user supplied cutoffs to plot as points. If non-NULL, it
  will override the values of n_cuts and plot the observed cutoffs
  closest to the user-supplied ones. Both `cutoffs_at` and
  `cutoffs.labels` will be passed to
  [`plotROC::geom_roc()`](https://sachsmc.github.io/plotROC/reference/geom_roc.html).
  Other than numeric values, the following special values are allowed.
  These values are the methods of
  [`OptimalCutpoints::optimal.cutpoints()`](https://rdrr.io/pkg/OptimalCutpoints/man/optimal.cutpoints.html).

- cutoffs_labels:

  vector of user-supplied labels for the cutoffs. Must be a character
  vector of the same length as cutoffs_at.

- cutoffs_accuracy:

  A numeric to specify the accuracy of the cutoff values to show.

- cutoffs_pt_size:

  A numeric to specify the size of the cutoff points.

- cutoffs_pt_shape:

  A numeric to specify the shape of the cutoff points.

- cutoffs_pt_stroke:

  A numeric to specify the stroke of the cutoff points.

- cutoffs_labal_fg:

  A character string to specify the color of the cutoff labels.

- cutoffs_label_size:

  A numeric to specify the size of the cutoff labels.

- cutoffs_label_bg:

  A character string to specify the background color of the cutoff
  labels.

- cutoffs_label_bg_r:

  A numeric to specify the radius of the background of the cutoff
  labels.

- show_auc:

  A character string to specify the position of the AUC values.

  - "auto" (default): Automatically determine the position based on the
    plot. When there is a single group or 'facet_by' is provided, the
    AUC is placed on the plot. Otherwise, the AUC is placed in the
    legend.

  - "none": Do not display the AUC values.

  - "legend": Display the AUC values in the legend.

  - "plot": Display the AUC values on the plot (left/right bottom
    corner).

- auc_accuracy:

  A numeric to specify the accuracy of the AUC values.

- auc_size:

  A numeric to specify the size of the AUC values when they are
  displayed on the plot.

- increasing:

  TRUE if the score is increasing with the truth (1), FALSE otherwise.

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

## Value

A ggplot object.
