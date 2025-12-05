# Calculate plot dimensions and layout

Main function for calculating optimal plot dimensions and layout
settings. Combines base dimensions, legend space, and coordinate system
settings.

## Usage

``` r
calculate_plot_layout(
  data,
  x,
  group_by,
  flip,
  stack,
  facet_by,
  facet_scales,
  facet_nrow,
  facet_ncol,
  legend.position,
  legend.direction,
  y_min_use,
  y_max_use
)
```

## Arguments

- data:

  Data frame containing the plot data

- x:

  Character string; x-axis column name

- group_by:

  Character string; grouping column name (can be NULL)

- flip:

  Logical; whether coordinates are flipped

- stack:

  Logical; whether facets are stacked

- facet_by:

  Character vector; faceting column(s)

- facet_scales:

  Character string; facet scales setting

- facet_nrow:

  Integer; number of facet rows (can be NULL)

- facet_ncol:

  Integer; number of facet columns (can be NULL)

- legend.position:

  Character string; legend position

- legend.direction:

  Character string; legend direction

- y_min_use:

  Numeric; y-axis minimum value

- y_max_use:

  Numeric; y-axis maximum value

## Value

List with width, height, facet_nrow, facet_ncol, strip_position, coord
