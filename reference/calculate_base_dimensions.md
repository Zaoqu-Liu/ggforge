# Calculate base plot dimensions

Calculates optimal plot dimensions based on plot orientation, stacking,
label length, and number of categories/groups.

## Usage

``` r
calculate_base_dimensions(flip, stack, x_maxchars, nx, nd)
```

## Arguments

- flip:

  Logical; whether coordinates are flipped

- stack:

  Logical; whether facets are stacked

- x_maxchars:

  Integer; maximum character length of x-axis labels

- nx:

  Integer; number of x-axis categories

- nd:

  Integer; number of groups (1 if no grouping)

## Value

List with width, height, facet_nrow, facet_ncol, strip_position

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard vertical boxplot
calculate_base_dimensions(
  flip = FALSE, stack = FALSE,
  x_maxchars = 10, nx = 5, nd = 2
)

# Horizontal boxplot with stacked facets
calculate_base_dimensions(
  flip = TRUE, stack = TRUE,
  x_maxchars = 15, nx = 4, nd = 1
)
} # }
```
