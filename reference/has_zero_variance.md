# Check if a vector has zero variance

Determines whether a numeric vector has zero variance (all values
identical or all NA). Used to prevent statistical test failures.

## Usage

``` r
has_zero_variance(vec)
```

## Arguments

- vec:

  Numeric vector to check

## Value

Logical; TRUE if vector has zero variance, FALSE otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
has_zero_variance(c(1, 1, 1, 1))
# [1] TRUE

has_zero_variance(c(1, 2, 3, 4))
# [1] FALSE

has_zero_variance(c(NA, NA, NA))
# [1] TRUE

has_zero_variance(c(5, NA, 5, NA))
# [1] TRUE
} # }
```
