# Detect Venn data type

Auto-detect the format of input data for Venn diagram based on structure

## Usage

``` r
detect_venn_datatype(data, group_by = NULL, id_by = NULL)
```

## Arguments

- data:

  A data frame, list, or VennPlotData object

- group_by:

  Column(s) for grouping

- id_by:

  Column for IDs

## Value

Character string: "long", "wide", "list", or "venn"
