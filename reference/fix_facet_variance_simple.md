# Fix variance issues in facet data for simple comparisons

Modifies facet data to ensure valid statistical comparisons for
non-grouped (simple) comparisons.

## Usage

``` r
fix_facet_variance_simple(facet_data, x, y)
```

## Arguments

- facet_data:

  Data frame containing facet data

- x:

  Character string; x-axis column name

- y:

  Character string; y-axis column name

## Value

Modified data frame with variance issues fixed
