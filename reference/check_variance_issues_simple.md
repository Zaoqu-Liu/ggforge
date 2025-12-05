# Check if data groups have variance issues for x comparisons

Examines data groups to detect zero-variance situations for simple
(non-grouped) comparisons.

## Usage

``` r
check_variance_issues_simple(data_groups, y)
```

## Arguments

- data_groups:

  List of data frames, each representing a group

- y:

  Character string; y-axis column name

## Value

Logical; TRUE if variance issues detected, FALSE otherwise
