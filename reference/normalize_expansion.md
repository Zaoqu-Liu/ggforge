# Normalize expansion values (CSS-like padding)

Normalize expansion values (CSS-like padding)

## Usage

``` r
normalize_expansion(
  expand,
  x_type,
  y_type,
  continuous_default = c(0.05, 0),
  discrete_default = c(0, 0.6)
)
```

## Arguments

- expand:

  Numeric vector of expansion values

- x_type:

  Type of x-axis ("continuous" or "discrete")

- y_type:

  Type of y-axis ("continuous" or "discrete")

- continuous_default:

  Default for continuous axes

- discrete_default:

  Default for discrete axes

## Value

List with x and y expansion values
