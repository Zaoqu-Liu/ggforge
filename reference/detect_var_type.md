# Detect variable type

Determines the type of a variable for styling purposes.

Type hierarchy:

- continuous: numeric (not factor)

- discrete: factor, character, logical

- temporal: Date, POSIXct, POSIXlt

- ordered: ordered factor

## Usage

``` r
detect_var_type(data, column)
```

## Arguments

- data:

  Data frame

- column:

  Character. Column name to check

## Value

Character. One of "continuous", "discrete", "temporal", "ordered"

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(
  x = 1:10,
  y = factor(letters[1:10]),
  z = Sys.Date() + 1:10
)
detect_var_type(data, "x") # "continuous"
detect_var_type(data, "y") # "discrete"
detect_var_type(data, "z") # "temporal"
} # }
```
