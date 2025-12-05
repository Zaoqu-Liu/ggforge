# Main ggforge Theme

The default theme for ggforge, providing a clean and modern appearance

## Usage

``` r
theme_ggforge(aspect.ratio = NULL, base_size = NULL, font_family = NULL, ...)
```

## Arguments

- aspect.ratio:

  Aspect ratio of the plot panel

- base_size:

  Base font size (scales all text elements)

- font_family:

  Font family for all text

- ...:

  Additional arguments passed to
  [`theme`](https://ggplot2.tidyverse.org/reference/theme.html)

## Value

A ggplot2 theme object

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme_ggforge()
```
