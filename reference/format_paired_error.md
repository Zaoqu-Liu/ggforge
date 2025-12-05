# Format paired validation error message

Generates a human-readable error message when paired data validation
fails.

## Usage

``` r
format_paired_error(
  problem_groups,
  x,
  paired_by,
  group_by = NULL,
  n_total_col,
  with_group = TRUE
)
```

## Arguments

- problem_groups:

  Data frame containing problematic group combinations

- x:

  Character string; x-axis column name

- paired_by:

  Character string; pairing column name

- group_by:

  Character string; grouping column name (can be NULL)

- n_total_col:

  Character string; name of the count column

- with_group:

  Logical; whether group_by is used

## Value

Character string containing the formatted error message
