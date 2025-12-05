# Validate paired data without groups

Validates that paired data without grouping has the correct structure:
exactly two unique x values, and each paired_by value appears exactly
once for each x value.

## Usage

``` r
validate_paired_simple(data, x, paired_by)
```

## Arguments

- data:

  Data frame to validate

- x:

  Character string; x-axis column name

- paired_by:

  Character string; pairing column name

## Value

NULL (invisibly); throws error if validation fails

## Examples

``` r
if (FALSE) { # \dontrun{
# Valid paired data
df <- data.frame(
  time = rep(c("Pre", "Post"), each = 5),
  subject = rep(1:5, 2),
  value = rnorm(10)
)
validate_paired_simple(df, "time", "subject")

# Invalid: more than 2 x values
df_invalid <- data.frame(
  time = rep(c("T1", "T2", "T3"), each = 5),
  subject = rep(1:5, 3),
  value = rnorm(15)
)
# validate_paired_simple(df_invalid, "time", "subject")
# Error: Exactly two unique values of 'x' are required
} # }
```
