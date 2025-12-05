# Add minimal variance to vector

Adds tiny perturbations to a constant vector to enable statistical
tests. This is used when data has zero variance but comparisons are
still needed.

## Usage

``` r
add_minimal_variance(yval, base_value = NULL)
```

## Arguments

- yval:

  Numeric vector to modify

- base_value:

  Optional base value for calculating epsilon

## Value

Modified numeric vector with minimal variance added

## Examples

``` r
# Constant vector
vec <- c(5, 5, 5, 5)
add_minimal_variance(vec)
#> Error in add_minimal_variance(vec): could not find function "add_minimal_variance"
# Returns approximately c(4.9999999995, 5.0000000005, 5, 5)

# Vector with NA
vec_na <- c(10, 10, NA, 10)
add_minimal_variance(vec_na)
#> Error in add_minimal_variance(vec_na): could not find function "add_minimal_variance"
# Adds variance only to non-NA values
```
