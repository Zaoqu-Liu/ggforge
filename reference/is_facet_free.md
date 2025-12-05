# Check if facets have free scales

Determines whether faceted plot has free scales on the relevant axis
(y-axis for non-flipped, x-axis for flipped plots).

## Usage

``` r
is_facet_free(facet_by, facet_scales, flip)
```

## Arguments

- facet_by:

  Character vector; faceting column(s), or NULL

- facet_scales:

  Character string; facet scales setting

- flip:

  Logical; whether coordinates are flipped

## Value

Logical; TRUE if facets have free scales on relevant axis

## Examples

``` r
if (FALSE) { # \dontrun{
# Free y-axis (non-flipped plot)
is_facet_free("panel", "free_y", flip = FALSE)
# [1] TRUE

# Free x-axis (flipped plot)
is_facet_free("panel", "free_x", flip = TRUE)
# [1] TRUE

# No faceting
is_facet_free(NULL, "free", flip = FALSE)
# [1] FALSE
} # }
```
