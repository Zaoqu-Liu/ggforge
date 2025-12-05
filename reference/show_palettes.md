# Show available palettes

Display available color palettes visually

## Usage

``` r
show_palettes(
  palettes = NULL,
  type = c("discrete", "continuous"),
  index = NULL,
  palette_names = NULL,
  return_names = TRUE,
  return_palettes = FALSE
)
```

## Arguments

- palettes:

  Custom palette list (NULL to use built-in)

- type:

  Type of palettes to show: "discrete", "continuous", or both

- index:

  Indices of palettes to show

- palette_names:

  Specific palette names to show

- return_names:

  Return palette names instead of plotting

- return_palettes:

  Return palette colors instead of plotting

## Value

Plot, palette names, or palette colors
