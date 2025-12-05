# Preprocess data for pairwise comparisons without group_by

Prepares data for simple (non-grouped) pairwise statistical comparisons
by detecting and fixing variance issues.

## Usage

``` r
preprocess_comparison_data_simple(data, x, y, facet_by = NULL)
```

## Arguments

- data:

  Data frame containing the plot data

- x:

  Character string; x-axis column name

- y:

  Character string; y-axis column name

- facet_by:

  Character vector; faceting column name(s), or NULL

## Value

Preprocessed data frame ready for statistical comparisons

## Examples

``` r
if (FALSE) { # \dontrun{
# Sample data
df <- data.frame(
  category = factor(rep(LETTERS[1:3], each = 10)),
  value = c(rnorm(10, 5), rnorm(10, 7), rnorm(10, 6))
)

# Preprocess for simple comparisons
df_processed <- preprocess_comparison_data_simple(df, "category", "value")
} # }
```
