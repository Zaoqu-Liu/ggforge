# Get or set ggforge options

Get or set ggforge options

## Usage

``` r
ggforge_option(...)
```

## Arguments

- ...:

  Named arguments to set options, or character strings to get options

## Value

If setting options, returns invisible NULL. If getting options, returns
the option value.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set options
ggforge_option(theme.base_size = 14, theme.font_family = "Arial")

# Get options
ggforge_option("theme.base_size")
} # }
```
