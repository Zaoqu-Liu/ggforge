# Wrap spatial plot layers

Wrap spatial plot layers

## Usage

``` r
.wrap_spatial_layers(
  layers,
  ext = NULL,
  flip_y = TRUE,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  theme = "theme_ggforge",
  theme_args = list()
)
```

## Arguments

- layers:

  List of ggplot layers

- ext:

  Spatial extent

- flip_y:

  Whether to flip y-axis

- legend.position:

  Legend position

- legend.direction:

  Legend direction

- title:

  Plot title

- subtitle:

  Plot subtitle

- xlab:

  X-axis label

- ylab:

  Y-axis label

- theme:

  Theme function or name

- theme_args:

  Theme arguments

## Value

A ggplot object
