# Detect the type of the input data for Upset plot

Automatically detects whether the input data is in long format, wide
format, list format, or an already processed UpsetPlotData object.

## Usage

``` r
detect_upset_datatype(data, group_by = NULL, id_by = NULL)
```

## Arguments

- data:

  A data frame or a list

- group_by:

  A character string specifying the column name of the data frame to
  group the data.

- id_by:

  A character string specifying the column name of the data frame to
  identify the instances. Required when `group_by` is a single column
  and data is a data frame.

## Value

A character string indicating the datatype of the input data or error
message if invalid. Possible values are "long", "wide", "list" and
"upset".

- "long": Data is in long format (group_by column with id_by column)

- "wide": Data is in wide format (multiple logical/binary columns)

- "list": Data is a named list of vectors

- "upset": Data is a pre-processed UpsetPlotData object
