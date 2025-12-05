# Validate paired data structure

Main validation function for paired data. Handles NA values in
paired_by, validates structure based on whether group_by is provided,
and sorts data for proper pairing.

## Usage

``` r
validate_paired_data(data, x, y, paired_by, group_by = NULL)
```

## Arguments

- data:

  Data frame to validate

- x:

  Character string; x-axis column name

- y:

  Character string; y-axis column name

- paired_by:

  Character string; pairing column name

- group_by:

  Character string; grouping column name (can be NULL)

## Value

Validated and sorted data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# Paired t-test scenario: before/after measurements
df <- data.frame(
  timepoint = factor(rep(c("Before", "After"), each = 10)),
  subject_id = factor(rep(1:10, 2)),
  measurement = c(rnorm(10, 100, 15), rnorm(10, 95, 15))
)

# Validate and sort data
df_validated <- validate_paired_data(df, "timepoint", "measurement", "subject_id")

# With group_by (e.g., treatment groups)
df_grouped <- data.frame(
  timepoint = rep(c("Before", "After"), 20),
  subject_id = factor(rep(1:20, each = 2)),
  treatment = factor(rep(c("Placebo", "Drug"), each = 20)),
  measurement = rnorm(40, 100, 15)
)
df_validated2 <- validate_paired_data(
  df_grouped, "timepoint", "measurement", "subject_id", "treatment"
)
} # }
```
