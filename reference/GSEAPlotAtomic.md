# GSEA Plot Atomic

Creates a detailed GSEA plot for a single gene set, showing the running
enrichment score, gene positions, and ranked list metric.

## Usage

``` r
GSEAPlotAtomic(
  data,
  gene_ranks = "@gene_ranks",
  gs,
  genes,
  metric = "p.adjust",
  sample_coregenes = FALSE,
  line_width = 1.5,
  line_alpha = 1,
  line_color = "#6BB82D",
  n_coregenes = 10,
  genes_label = NULL,
  label_fg = "black",
  label_bg = "white",
  label_bg_r = 0.1,
  label_size = 4,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- gene_ranks:

  A numeric vector of gene ranks with genes as names The gene ranks are
  used to plot the gene sets. If `gene_ranks` is a character vector
  starting with `@`, the gene ranks will be taken from the attribute of
  `data`.

- gs:

  The name of the gene set

- genes:

  The genes in the gene set

- metric:

  The metric to show in the subtitle

- sample_coregenes:

  A logical value to sample the core genes from the core_enrichment; if
  `FALSE`, the first `n_coregenes` will be used

- line_width:

  The width of the line in the running score plot

- line_alpha:

  The alpha of the line in the running score plot

- line_color:

  The color of the line in the running score plot

- n_coregenes:

  The number of core genes to label

- genes_label:

  The genes to label. If set, `n_coregenes` will be ignored

- label_fg:

  The color of the label text

- label_bg:

  The background color of the label

- label_bg_r:

  The radius of the background color of the label

- label_size:

  The size of the label text

- title:

  Plot title

- subtitle:

  Plot subtitle

- xlab:

  X-axis label

- ylab:

  The label of the y-axis, will be shown on the right side
