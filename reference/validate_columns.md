# Column Validator

Validates that specified columns exist in a data frame and optionally
converts them to factors with proper level ordering.

## Usage

``` r
validate_columns(
  data,
  columns,
  force_factor = FALSE,
  allow_multi = FALSE,
  concat_multi = FALSE,
  concat_sep = "_"
)
```

## Arguments

- data:

  A data frame

- columns:

  Column name(s) to validate

- force_factor:

  Whether to convert to factor

- allow_multi:

  Whether to allow multiple columns

- concat_multi:

  Whether to concatenate multiple columns

- concat_sep:

  Separator for concatenation

## Value

Validated column name(s)
