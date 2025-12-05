# Add Point Layers (Normal + Highlight)

Unified function to add point layers with optional highlighting. Handles
both regular geom_point and raster mode (scattermore) for large
datasets. Highlighted points are rendered on top with custom styling.

## Usage

``` r
add_point_layers(
  p,
  data,
  x,
  y,
  group_by,
  pt_size,
  pt_shape,
  alpha,
  raster,
  raster_dpi,
  highlight_color,
  highlight_size,
  highlight_alpha,
  highlight_stroke
)
```

## Arguments

- p:

  ggplot object

- data:

  Data frame with .highlight column

- x:

  X variable name

- y:

  Y variable name

- group_by:

  Grouping variable for coloring

- pt_size:

  Point size for normal points

- pt_shape:

  Point shape (0-25)

- alpha:

  Alpha transparency for normal points

- raster:

  Use raster graphics (scattermore)

- raster_dpi:

  DPI for raster mode

- highlight_color:

  Color for highlighted points

- highlight_size:

  Size for highlighted points

- highlight_alpha:

  Alpha for highlighted points

- highlight_stroke:

  Stroke width for highlighted points

## Value

ggplot object with point layers added
