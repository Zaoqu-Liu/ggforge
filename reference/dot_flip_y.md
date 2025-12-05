# Flip y-coordinates for spatial data

Internal functions to flip y-coordinates of spatial objects. This is
useful for visualizing spatial data with the origin at the top left
corner.

## Usage

``` r
.flip_y(data, ...)

# S3 method for class 'SpatRaster'
.flip_y(data, ...)

# S3 method for class 'SpatVector'
.flip_y(data, ...)

# S3 method for class 'data.frame'
.flip_y(data, y = "y", ...)
```

## Arguments

- data:

  A SpatRaster, SpatVector, or data.frame object

- ...:

  Additional arguments

- y:

  Column name for y-coordinates (data.frame only)

## Value

Object with flipped y-coordinates
