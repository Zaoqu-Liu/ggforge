# Validate numeric range

Validate numeric range

## Usage

``` r
validate_numeric_range(
  value,
  param_name,
  min_val = -Inf,
  max_val = Inf,
  allow_null = TRUE
)
```

## Arguments

- value:

  Value to validate

- param_name:

  Parameter name for error message

- min_val:

  Minimum allowed value

- max_val:

  Maximum allowed value

- allow_null:

  Whether NULL is allowed

## Value

The validated value
