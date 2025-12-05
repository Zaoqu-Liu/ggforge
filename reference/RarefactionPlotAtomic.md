# Rarefaction Plot Atomic

Creates a single rarefaction plot without splitting

## Usage

``` r
RarefactionPlotAtomic(
  data,
  type = 1,
  se = TRUE,
  group_by = "group",
  group_name = NULL,
  pt_size = 3,
  line_width = 1,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  alpha = 0.2,
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

  An iNEXT object or a list of data that will be handled by
  [iNEXT::iNEXT](https://rdrr.io/pkg/iNEXT/man/iNEXT.html).

- type:

  Three types of plots: sample-size-based rarefaction/extrapolation
  curve (`type = 1`); sample completeness curve (`type = 2`);
  coverage-based rarefaction/extrapolation curve (`type = 3`).

- se:

  A logical variable to display confidence interval around the estimated
  sampling curve. Default to `NULL` which means TRUE if the data has the
  lower and upper bounds.

- group_by:

  A character string indicating how to group the data (color the lines).
  Possible values are "q" and "group"

- group_name:

  A character string indicating the name of the group, showing as the
  legend title.

- pt_size:

  A numeric value specifying the size of the points.

- line_width:

  A numeric value specifying the width of the lines.

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

  A character string indicating how to facet the data and plots.
  Possible values are "q" and "group"

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

- ...:

  Additional arguments to pass to
  [iNEXT::iNEXT](https://rdrr.io/pkg/iNEXT/man/iNEXT.html) when `data`
  is not an iNEXT object.
