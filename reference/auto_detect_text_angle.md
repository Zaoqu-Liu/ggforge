# Auto-detect optimal x-axis text angle based on label length

Automatically determines the best angle for x-axis labels based on the
maximum label length, number of labels, and plot orientation.

## Usage

``` r
auto_detect_text_angle(data, x, flip = FALSE, stack = FALSE, n_groups = 1)
```

## Arguments

- data:

  A data frame containing the plot data

- x:

  Character string specifying the x-axis column name

- flip:

  Logical; whether the plot coordinates are flipped

- stack:

  Logical; whether facets are stacked

- n_groups:

  Integer; number of groups (for dodged plots)

## Value

Numeric value (0, 45, or 90) representing the optimal text angle

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample data
df <- data.frame(
  short_labels = factor(c("A", "B", "C")),
  long_labels = factor(c("Category One", "Category Two", "Category Three"))
)

# Short labels - returns 0 (horizontal)
auto_detect_text_angle(df, "short_labels", flip = FALSE, stack = FALSE)

# Long labels - returns 45 (angled)
auto_detect_text_angle(df, "long_labels", flip = FALSE, stack = FALSE)
} # }
```
