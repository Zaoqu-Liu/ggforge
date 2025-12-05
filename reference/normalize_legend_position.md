# Normalize legend position (handle waiver)

Handles waiver objects for legend.position, converting them to actual
positions based on whether a grouping variable exists.

## Usage

``` r
normalize_legend_position(
  position,
  has_group,
  default_with_group = "right",
  default_no_group = "none"
)
```

## Arguments

- position:

  Legend position (can be waiver or string)

- has_group:

  Logical, whether a grouping variable exists

- default_with_group:

  Default position when group exists

- default_no_group:

  Default position when no group

## Value

Normalized position string
