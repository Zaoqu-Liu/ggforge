# Check linewidth parameter (compatibility shim for qqplotr)

This is a compatibility shim for the qqplotr package which expects the
check_linewidth() function from older versions of ggplot2. This function
handles the conversion of the old 'size' aesthetic to the new
'linewidth' aesthetic.

## Usage

``` r
check_linewidth(data, snake_class = NULL)
```

## Arguments

- data:

  Data frame containing aesthetic data

- snake_class:

  Class name in snake_case (unused, for compatibility)

## Value

Data frame with size converted to linewidth if needed
