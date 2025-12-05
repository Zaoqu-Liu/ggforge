# Atomic Enrichment Network

Creates a single enrichment network plot showing connections between
terms and genes

## Usage

``` r
EnrichNetworkAtomic(
  data,
  top_term = 6,
  metric = "p.adjust",
  character_width = 50,
  layout = "fr",
  layoutadjust = TRUE,
  adjscale = 60,
  adjiter = 100,
  blendmode = "blend",
  labelsize = 5,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525,
  ...
)
```

## Arguments

- data:

  A data frame containing the data to be plotted. It should be in the
  format of clusterProfiler enrichment result, which includes the
  columns: ID, Description, GeneRatio, BgRatio, pvalue, p.adjust,
  qvalue, geneID and Count.

  - The `ID`, `qvalue`, `BgRatio`, and `Count` columns are optional.

  - The `Description` is the description of the term.

  - The `GeneRatio` is the number of genes in the term divided by the
    total number of genes in the input list.

  - The `BgRatio` is the number of genes in the term divided by the
    total number of genes in the background list (all terms).

  - The `Count` column, if given, should be the same as the first number
    in GeneRatio.

- top_term:

  An integer specifying the number of top terms to show.

- metric:

  A character string specifying the metric to use for the size of the
  nodes. It is also used to order the terms when selected the top terms.
  Either "pvalue" or "p.adjust". The default is "p.adjust".

- character_width:

  The width of the characters used to wrap the keyword.

- layout:

  A character string specifying the layout of the graph. Either
  "circle", "tree", "grid" or other layout functions in `igraph`.

- layoutadjust:

  A logical value specifying whether to adjust the layout of the
  network.

- adjscale:

  A numeric value specifying the scale of the adjustment.

- adjiter:

  A numeric value specifying the number of iterations for the
  adjustment.

- blendmode:

  A character string specifying the blend mode of the colors. Either
  "blend", "average", "multiply" and "screen".

- labelsize:

  A numeric value specifying the size of the label.

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

- aspect.ratio:

  Aspect ratio of plot panel

- legend.position:

  Legend position: "none", "left", "right", "bottom", "top"

- legend.direction:

  Legend direction: "horizontal" or "vertical"

- title:

  Plot title

- subtitle:

  Plot subtitle

- xlab:

  X-axis label

- ylab:

  Y-axis label

- seed:

  Random seed for reproducibility

## Value

A ggplot object
