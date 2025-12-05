# Calculate Annotation Item

Helper function to calculate and format individual annotation items for
correlation plots. Supports equation, R², p-value, and correlation
coefficients.

## Usage

``` r
calculate_annotation(item, dat, x, y, m)
```

## Arguments

- item:

  Annotation item type: "eq", "r2", "p", "spearman", "pearson",
  "kendall", "n"

- dat:

  Data frame with x and y variables

- x:

  X variable name

- y:

  Y variable name

- m:

  Linear model object (lm result)

## Value

Character string of formatted annotation expression (plotmath format)
