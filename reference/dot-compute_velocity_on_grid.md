# Compute velocity on grid

Internal function to compute velocity vectors on a grid for
visualization. Based on scvelo's velocity_embedding_grid implementation.

## Usage

``` r
.compute_velocity_on_grid(
  embedding,
  v_embedding,
  density = NULL,
  smooth = NULL,
  n_neighbors = NULL,
  min_mass = NULL,
  scale = 1,
  adjust_for_stream = FALSE,
  cutoff_perc = NULL
)
```

## Arguments

- embedding:

  Matrix of n_obs x n_dim with cell embedding coordinates

- v_embedding:

  Matrix of n_obs x n_dim with velocity vectors

- density:

  Density of grid points along each dimension

- smooth:

  Smoothing factor for velocity vectors

- n_neighbors:

  Number of nearest neighbors for each grid point

- min_mass:

  Minimum mass required for a grid point

- scale:

  Scaling factor for velocity vectors

- adjust_for_stream:

  Whether to adjust for streamlines

- cutoff_perc:

  Percentile cutoff for removing low-density grid points

## Value

List with x_grid and v_grid components
