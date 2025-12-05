# Check if data groups have variance issues for group_by comparisons

Examines grouped data to detect zero-variance situations that would
cause statistical comparison failures.

## Usage

``` r
check_variance_issues_grouped(data_groups, x, y, group_by)
```

## Arguments

- data_groups:

  List of data frames, each representing a group

- x:

  Character string; x-axis column name

- y:

  Character string; y-axis column name

- group_by:

  Character string; grouping column name

## Value

Logical; TRUE if variance issues detected, FALSE otherwise
