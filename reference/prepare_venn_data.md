# Prepare Venn data

Prepare data for Venn diagram plotting. Converts various input formats
to VennPlotData object used by ggVennDiagram.

Input formats:

- Long format: group_by and id_by columns

        group_by id_by
        A        a1
        A        a2
        B        a1
        B        a3
        

- Wide format: Logical or 0/1 columns

        A     B
        TRUE  TRUE
        TRUE  FALSE
        FALSE TRUE
        

- List format: Named list of character vectors

        list(A = c("a1", "a2"), B = c("a1", "a3"))
        

## Usage

``` r
prepare_venn_data(
  data,
  in_form = "auto",
  group_by = NULL,
  group_by_sep = "_",
  id_by = NULL
)
```

## Arguments

- data:

  Input data

- in_form:

  Data format ("auto", "long", "wide", "list", or "venn")

- group_by:

  Column(s) for grouping

- group_by_sep:

  Separator for concatenating multiple columns

- id_by:

  Column containing IDs

## Value

VennPlotData object
