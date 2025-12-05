# Clustree Plot Atomic

Creates a single clustree plot without splitting. This is the internal
function that generates the actual visualization.

## Usage

``` r
ClustreePlotAtomic(
  data,
  prefix,
  flip = FALSE,
  alpha = 0.85,
  palette = "Paired",
  palcolor = NULL,
  edge_palette = "Spectral",
  edge_palcolor = NULL,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  expand = c(0.1, 0.1),
  theme = "theme_ggforge",
  theme_args = list(),
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- alpha:

  Transparency level (0-1)

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

- xlab:

  X-axis label

- ylab:

  Y-axis label

- expand:

  Expansion values for plot axes (CSS-like padding)

- theme:

  Theme name (string) or theme function

- theme_args:

  List of arguments passed to theme function
