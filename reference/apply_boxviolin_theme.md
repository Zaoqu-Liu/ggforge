# Apply plot theme with consistent defaults

Applies a consistent theme to box/violin plots with optimized settings
for readability and aesthetics. Handles font sizing, grid lines,
spacing, and axis text orientation.

## Usage

``` r
apply_boxviolin_theme(
  p,
  theme,
  theme_args,
  layout_info,
  aspect.ratio,
  legend.position,
  legend.direction,
  x_text_angle,
  just,
  flip,
  stack,
  data,
  x,
  y
)
```

## Arguments

- p:

  ggplot object to modify

- theme:

  Theme function to use

- theme_args:

  List of additional theme arguments

- layout_info:

  List containing layout information

- aspect.ratio:

  Numeric; plot aspect ratio

- legend.position:

  Character string; legend position

- legend.direction:

  Character string; legend direction

- x_text_angle:

  Numeric; x-axis text angle (0, 45, or 90)

- just:

  List with h and v justification values

- flip:

  Logical; whether coordinates are flipped

- stack:

  Logical; whether facets are stacked

- data:

  Data frame containing the plot data

- x:

  Character string; x-axis column name

- y:

  Character string; y-axis column name

## Value

Modified ggplot object with theme applied
