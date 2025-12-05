# Apply data-driven styling to plot

Automatically applies axis and legend styling based on variable types.
This is the core function that eliminates the need for manual
get_axis_text_size() calls in every plot function.

## Usage

``` r
apply_style_theme(
  plot,
  data,
  x_var = NULL,
  y_var = NULL,
  flip = FALSE,
  base_size = 12,
  ...
)
```

## Arguments

- plot:

  ggplot object

- data:

  Data frame used in the plot

- x_var:

  Character. X variable name (NULL if not applicable)

- y_var:

  Character. Y variable name (NULL if not applicable)

- flip:

  Logical. Whether axes are flipped

- base_size:

  Numeric. Base font size

- ...:

  Additional theme arguments to merge

## Value

ggplot object with styling applied

## Examples

``` r
if (FALSE) { # \dontrun{
p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = factor(cyl))) +
  ggplot2::geom_point()

# Automatic styling based on variable types
p <- apply_style_theme(p, mtcars, x_var = "mpg", y_var = "cyl")
} # }
```
