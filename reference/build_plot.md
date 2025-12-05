# Build a plot with standard workflow

This is the core function that handles the standard plot workflow:

1.  Validate parameters

2.  Split data if requested

3.  Build atomic plots

4.  Combine or facet plots

## Usage

``` r
build_plot(
  data,
  atomic_fn,
  params,
  split_by = NULL,
  facet_by = NULL,
  combine = TRUE,
  ...
)
```

## Arguments

- data:

  Data frame

- atomic_fn:

  Function to create atomic plot

- params:

  List of parameters

- split_by:

  Column to split by

- facet_by:

  Column to facet by

- combine:

  Whether to combine plots

- ...:

  Additional arguments

## Value

Plot or list of plots
