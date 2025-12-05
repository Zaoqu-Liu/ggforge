# Adjust Network Layout

Internal helper function for adjusting network layout to prevent node
overlap

## Usage

``` r
adjust_network_layout(
  graph,
  layout,
  width,
  height = 2,
  scale = 100,
  iter = 100
)
```

## Arguments

- graph:

  igraph object

- layout:

  Initial layout matrix

- width:

  Width values for nodes

- height:

  Height for vertical spacing

- scale:

  Scaling factor for layout

- iter:

  Number of iterations for adjustment

## Value

Adjusted layout matrix
