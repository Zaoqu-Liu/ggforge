# Get style specification by path

Retrieves a style specification using dot-notation path. Automatically
scales font sizes based on base_size.

## Usage

``` r
get_style_spec(path, base_size = 12)
```

## Arguments

- path:

  Character. Dot-separated path (e.g., "font.axis_text.continuous")

- base_size:

  Numeric. Base font size for scaling (default: 12)

## Value

The specification value (list or atomic)

## Examples

``` r
if (FALSE) { # \dontrun{
get_style_spec("font.title")
get_style_spec("font.axis_text.continuous", base_size = 14)
} # }
```
