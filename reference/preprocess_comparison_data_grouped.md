# Preprocess data for pairwise comparisons with group_by

Prepares data for grouped pairwise statistical comparisons by detecting
and fixing variance issues that would cause test failures.

## Usage

``` r
preprocess_comparison_data_grouped(data, x, y, group_by, facet_by = NULL)
```

## Arguments

- data:

  Data frame containing the plot data

- x:

  Character string; x-axis column name

- y:

  Character string; y-axis column name

- group_by:

  Character string; grouping column name

- facet_by:

  Character vector; faceting column name(s), or NULL

## Value

Preprocessed data frame ready for statistical comparisons

## Examples

``` r
if (FALSE) { # \dontrun{
# Sample data with potential zero-variance groups
df <- data.frame(
  treatment = rep(c("A", "B"), each = 10),
  response = c(rep(5, 5), rnorm(5, 5), rep(3, 5), rnorm(5, 3)),
  group = rep(c("Control", "Test"), 10)
)

# Preprocess for grouped comparisons
df_processed <- preprocess_comparison_data_grouped(
  df, "treatment", "response", "group"
)
} # }
```
