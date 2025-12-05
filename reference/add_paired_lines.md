# Add paired lines to plot

Adds connecting lines between paired observations in box/violin plots.
Supports both simple pairing (connecting across x categories) and
grouped pairing (connecting within x categories across groups).

## Usage

``` r
add_paired_lines(p, data, x, y, paired_by, group_by, pt_color, pt_alpha)
```

## Arguments

- p:

  ggplot object to modify

- data:

  Data frame containing the plot data

- x:

  Character string; x-axis column name

- y:

  Character string; y-axis column name

- paired_by:

  Character string; pairing column name

- group_by:

  Character string; grouping column name (can be NULL)

- pt_color:

  Character string; line color

- pt_alpha:

  Numeric; line transparency (0-1)

## Value

Modified ggplot object with paired lines added

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Simple paired data (before/after)
df <- data.frame(
  time = factor(rep(c("Before", "After"), each = 10)),
  subject = factor(rep(1:10, 2)),
  value = c(rnorm(10, 100), rnorm(10, 95))
)

p <- ggplot(df, aes(x = time, y = value)) +
  geom_boxplot()

# Add paired lines (internal function call)
p <- add_paired_lines(p, df, "time", "value", "subject", NULL, "grey50", 0.5)
} # }
```
