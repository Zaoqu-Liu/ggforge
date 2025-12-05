# Validate and normalize common plot arguments

Validates common arguments used across all plot types

## Usage

``` r
validate_common_args(
  seed = 8525,
  facet_by = NULL,
  split_by = NULL,
  group_by = NULL,
  facet_scales = "fixed",
  theme = "theme_ggforge",
  palette = "Paired",
  alpha = 1,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  ...
)
```

## Arguments

- seed:

  Random seed

- facet_by:

  Faceting columns

- split_by:

  Split columns

- group_by:

  Grouping columns

- facet_scales:

  Facet scales type

- theme:

  Theme name or function

- palette:

  Palette name

- alpha:

  Transparency value

- aspect.ratio:

  Aspect ratio

- legend.position:

  Legend position

- legend.direction:

  Legend direction

- ...:

  Additional arguments (ignored)

## Value

Invisible NULL (sets seed as side effect)
