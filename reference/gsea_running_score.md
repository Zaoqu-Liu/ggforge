# Get the running enrichment score of a gene set

Calculates the GSEA running enrichment score using the weighted
Kolmogorov-Smirnov-like statistic

## Usage

``` r
gsea_running_score(genes, gene_ranks, exponent = 1, hits_only = TRUE)
```

## Arguments

- genes:

  A vector of genes in the gene set

- gene_ranks:

  A numeric vector of gene ranks with gene names

- exponent:

  A numeric value to raise the gene ranks to (default: 1)

- hits_only:

  A logical value to return only the running enrichment score of the
  hits (default: TRUE)

## Value

A numeric vector of the running enrichment score
