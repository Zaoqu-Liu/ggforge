# Chord Plot Atomic

Creates a single chord diagram without splitting. This is an internal
function called by
[`ChordPlot`](https://zaoqu-liu.github.io/ggforge/reference/chordplot.md)
for each split level.

## Usage

``` r
ChordPlotAtomic(
  data,
  y = NULL,
  from = NULL,
  from_sep = "_",
  to = NULL,
  to_sep = "_",
  flip = FALSE,
  links_color = c("from", "to"),
  labels_rot = FALSE,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 0.5,
  title = NULL,
  subtitle = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- y:

  Column for relationship values (if NULL, counts relationships)

- from:

  Column(s) for source nodes

- from_sep:

  Separator for concatenating multiple from columns

- to:

  Column(s) for target nodes

- to_sep:

  Separator for concatenating multiple to columns

- flip:

  Logical. Whether to flip source and target nodes

- links_color:

  Character. Color links by source ("from") or target ("to")

- labels_rot:

  Logical. Whether to rotate labels by 90 degrees

- theme:

  Theme name (string) or theme function

- theme_args:

  List of arguments passed to theme function

- palette:

  Color palette name

- palcolor:

  Custom colors for palette

- alpha:

  Transparency level (0-1)

- title:

  Plot title

- subtitle:

  Plot subtitle

- ...:

  Additional arguments (not currently used)

## Value

A patchwork-wrapped chord diagram with height/width attributes
