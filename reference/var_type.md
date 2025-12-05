# Annotate variable with explicit type

Allows users to override automatic type detection.

## Usage

``` r
var_type(x, type = c("continuous", "discrete", "temporal", "ordered"))
```

## Arguments

- x:

  Vector. The variable to annotate

- type:

  Character. One of "continuous", "discrete", "temporal", "ordered"

## Value

The vector with type attribute attached

## Examples

``` r
if (FALSE) { # \dontrun{
# Force a numeric year column to be treated as discrete
data$year <- var_type(data$year, "discrete")

# Force a character column to be treated as continuous (rare)
data$score <- var_type(as.numeric(data$score), "continuous")
} # }
```
