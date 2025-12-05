# Fix variance issues in facet data for grouped comparisons

Modifies facet data to ensure valid statistical comparisons can be
performed when zero-variance groups are detected.

## Usage

``` r
fix_facet_variance_grouped(facet_data, x, y, group_by)
```

## Arguments

- facet_data:

  Data frame containing facet data

- x:

  Character string; x-axis column name

- y:

  Character string; y-axis column name

- group_by:

  Character string; grouping column name

## Value

Modified data frame with variance issues fixed
