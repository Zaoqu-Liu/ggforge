# Box/Violin Plot Atomic

Creates a single box or violin plot without splitting. This is the core
plotting function that handles all the actual plot construction.

## Usage

``` r
BoxViolinPlotAtomic(
  data,
  x,
  x_sep = "_",
  y = NULL,
  base = c("box", "violin"),
  in_form = c("long", "wide"),
  sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc",
    "median"),
  flip = FALSE,
  keep_empty = FALSE,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  paired_by = NULL,
  x_text_angle = NULL,
  step_increase = 0.1,
  fill_mode = ifelse(!is.null(group_by), "dodge", "x"),
  fill_reverse = FALSE,
  symnum_args = NULL,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  aspect.ratio = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  add_point = FALSE,
  pt_color = "grey30",
  pt_size = NULL,
  pt_alpha = 1,
  y_nbreaks = 4,
  jitter_width = NULL,
  jitter_height = 0,
  stack = FALSE,
  y_max = NULL,
  y_min = NULL,
  y_trans = "identity",
  add_box = FALSE,
  box_color = "black",
  box_width = 0.1,
  box_ptsize = 2.5,
  add_trend = FALSE,
  trend_color = NULL,
  trend_linewidth = 1,
  trend_ptsize = 2,
  add_stat = NULL,
  stat_name = NULL,
  stat_color = "black",
  stat_size = 1,
  stat_stroke = 1,
  stat_shape = 25,
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  add_line = NULL,
  line_color = "red2",
  line_width = 0.6,
  line_type = 2,
  highlight = NULL,
  highlight_color = "red2",
  highlight_size = 1,
  highlight_alpha = 1,
  comparisons = NULL,
  ref_group = NULL,
  pairwise_method = "wilcox.test",
  multiplegroup_comparisons = FALSE,
  multiple_method = "kruskal.test",
  sig_label = "p.format",
  sig_labelsize = 3.5,
  hide_ns = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525,
  ...
)
```

## Details

This internal function is responsible for:

- Data preparation and validation

- Building the base ggplot object

- Adding layers (points, trend lines, statistical comparisons)

- Applying themes and formatting

- Calculating optimal dimensions

For typical usage, use the wrapper functions
[`BoxPlot()`](https://zaoqu-liu.github.io/ggforge/reference/BoxPlot.md)
or
[`ViolinPlot()`](https://zaoqu-liu.github.io/ggforge/reference/ViolinPlot.md)
instead.
