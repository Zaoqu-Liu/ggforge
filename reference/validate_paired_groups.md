# Validate paired data with groups

Validates that paired data with grouping has the correct structure: each
combination of x and paired_by should have exactly two observations, one
for each group.

## Usage

``` r
validate_paired_groups(data, x, paired_by, group_by)
```

## Arguments

- data:

  Data frame to validate

- x:

  Character string; x-axis column name

- paired_by:

  Character string; pairing column name

- group_by:

  Character string; grouping column name

## Value

NULL (invisibly); throws error if validation fails

## Examples

``` r
if (FALSE) { # \dontrun{
# Valid paired data with groups
df <- data.frame(
  time = rep(c("Before", "After"), each = 6),
  subject = rep(1:6, 2),
  group = rep(c("Control", "Treatment"), 6),
  value = rnorm(12)
)
validate_paired_groups(df, "time", "subject", "group")

# Invalid data will throw an error
df_invalid <- df[-1, ] # Remove one observation
# validate_paired_groups(df_invalid, "time", "subject", "group")
# Error: validation fails
} # }
```
