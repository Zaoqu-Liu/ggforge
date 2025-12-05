# Venn Diagram Atomic

Creates a single Venn diagram without splitting

## Usage

``` r
VennDiagramAtomic(
  data,
  in_form = "auto",
  group_by = NULL,
  group_by_sep = "_",
  id_by = NULL,
  label = "count",
  label_fg = "black",
  label_size = NULL,
  label_bg = "white",
  label_bg_r = 0.1,
  fill_mode = "count",
  fill_name = NULL,
  palette = "Spectral",
  palcolor = NULL,
  alpha = 1,
  theme = "theme_ggforge",
  theme_args = list(),
  title = NULL,
  subtitle = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- group_by:

  Column name(s) for grouping data

- group_by_sep:

  Separator when concatenating multiple group_by columns

- palette:

  Color palette name

- palcolor:

  Custom colors for palette

- alpha:

  Transparency level (0-1)

- theme:

  Theme name (string) or theme function

- theme_args:

  List of arguments passed to theme function

- title:

  Plot title

- subtitle:

  Plot subtitle

- legend.position:

  Legend position: "none", "left", "right", "bottom", "top"

- legend.direction:

  Legend direction: "horizontal" or "vertical"
