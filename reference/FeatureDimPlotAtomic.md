# Feature Dimension Reduction Plot Atomic

Feature Dimension Reduction Plot Atomic

## Usage

``` r
FeatureDimPlotAtomic(
  data,
  dims,
  features,
  lower_quantile = 0,
  upper_quantile = 0.99,
  lower_cutoff = NULL,
  upper_cutoff = NULL,
  bg_cutoff = NULL,
  color_name = "",
  pt_size = NULL,
  pt_alpha = 1,
  bg_color = "grey80",
  order = c("as-is", "reverse", "high-top", "low-top", "random"),
  show_stat = TRUE,
  highlight = NULL,
  highlight_alpha = 1,
  highlight_size = 1,
  highlight_color = "black",
  highlight_stroke = 0.8,
  add_density = FALSE,
  density_color = "grey80",
  density_filled = FALSE,
  density_filled_palette = "Greys",
  density_filled_palcolor = NULL,
  raster = NULL,
  raster_dpi = c(512, 512),
  hex = FALSE,
  hex_linewidth = 0.5,
  hex_count = FALSE,
  hex_bins = 50,
  hex_binwidth = NULL,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  theme = "theme_ggforge_grid",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  alpha = 1,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  ...
)
```
