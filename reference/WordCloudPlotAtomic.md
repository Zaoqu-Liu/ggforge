# Word Cloud Plot (Atomic)

Creates a single word cloud plot without data splitting. Visualizes word
frequency with size and color.

## Usage

``` r
WordCloudPlotAtomic(
  data,
  word_by = NULL,
  sentence_by = NULL,
  count_by = NULL,
  score_by = NULL,
  count_name = NULL,
  score_name = NULL,
  words_excluded = ggforge::words_excluded,
  score_agg = mean,
  minchar = 2,
  word_size = c(2, 8),
  top_words = 100,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  theme = "theme_ggforge",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  alpha = 1,
  palreverse = FALSE,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  seed = 8525,
  ...
)
```

## Arguments

- data:

  A data frame containing the data to plot

- word_by:

  Column name to use as the word (character column).

- sentence_by:

  Column name to split sentences into words (character column). Either
  word_by or sentence_by should be specified.

- count_by:

  Column name for word/sentence counts (numeric). If NULL, counts will
  be computed.

- score_by:

  Column name for word scores (numeric), used for color. If NULL, score
  will be set to 1.

- count_name:

  Name for the count legend.

- score_name:

  Name for the score legend.

- words_excluded:

  Character vector of words to exclude from word cloud.

- score_agg:

  Function to aggregate scores (default: mean).

- minchar:

  Minimum number of characters for words.

- word_size:

  Numeric vector specifying range of word sizes.

- top_words:

  Number of top words to show.

- facet_by:

  Column name(s) for faceting the plot

- facet_scales:

  Scales for facets: "fixed", "free", "free_x", "free_y"

- facet_ncol:

  Number of columns in facet layout

- facet_nrow:

  Number of rows in facet layout

- facet_byrow:

  Fill facets by row (TRUE) or column (FALSE)

- theme:

  Theme name (string) or theme function

- theme_args:

  List of arguments passed to theme function

- palette:

  Color palette name

- palcolor:

  Custom colors for palette

- alpha:

  Transparency level (0-1)

- palreverse:

  Whether to reverse the palette colors.

- aspect.ratio:

  Aspect ratio of plot panel

- legend.position:

  Legend position: "none", "left", "right", "bottom", "top"

- legend.direction:

  Legend direction: "horizontal" or "vertical"

- title:

  Plot title

- subtitle:

  Plot subtitle

- seed:

  Random seed for reproducibility

## Value

A ggplot object
