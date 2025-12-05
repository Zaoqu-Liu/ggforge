# Atomic Enrichment Map

Creates a single enrichment map plot showing clustering of enriched
terms

## Usage

``` r
EnrichMapAtomic(
  data,
  in_form = "clusterProfiler",
  top_term = 100,
  metric = "p.adjust",
  layout = "fr",
  minchar = 2,
  cluster = "fast_greedy",
  show_keyword = FALSE,
  nlabel = 4,
  character_width = 50,
  words_excluded = NULL,
  mark = "ellipse",
  label = c("term", "feature"),
  labelsize = 5,
  expand = c(0.4, 0.4),
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

- in_form:

  Input format ("clusterProfiler" or "enrichr")

- top_term:

  An integer specifying the number of top terms to show.

- metric:

  A character string specifying the metric to use for the size of the
  nodes. It is also used to order the terms when selected the top terms.
  Either "pvalue" or "p.adjust". The default is "p.adjust".

- layout:

  A character string specifying the layout of the graph. Either
  "circle", "tree", "grid" or other layout functions in `igraph`.

- minchar:

  An integer specifying the minimum number of characters to show in the
  keyword.

- cluster:

  A character string specifying the clustering method. Either
  "fast_greedy", "walktrap", "edge_betweenness", "infomap" or other
  clustering functions in `igraph`.

- show_keyword:

  A logical value specifying whether to show the keyword instead of
  Description/Term in the plot.

- nlabel:

  An integer specifying the number of labels to show in each cluster.

- character_width:

  The width of the characters used to wrap the keyword.

- words_excluded:

  A character vector specifying the words to exclude in the keyword.

- mark:

  A character string specifying the mark to use for the nodes. Either
  "ellipse", "rect", "circle", "text" or other mark functions in
  `ggforce`.

- label:

  A character string specifying the label to show in the legend. Either
  "term" or "feature". The default is "term".

- labelsize:

  A numeric value specifying the size of the label.

- expand:

  A numeric vector of length 2 specifying the expansion of the x and y
  axis.

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
