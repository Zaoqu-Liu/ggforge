#' Word Cloud Plot (Atomic)
#'
#' @description
#' Creates a single word cloud plot without data splitting.
#' Visualizes word frequency with size and color.
#'
#' @inheritParams parameters
#' @param word_by Column name to use as the word (character column).
#' @param sentence_by Column name to split sentences into words (character column).
#'   Either word_by or sentence_by should be specified.
#' @param count_by Column name for word/sentence counts (numeric).
#'   If NULL, counts will be computed.
#' @param score_by Column name for word scores (numeric), used for color.
#'   If NULL, score will be set to 1.
#' @param count_name Name for the count legend.
#' @param score_name Name for the score legend.
#' @param words_excluded Character vector of words to exclude from word cloud.
#' @param score_agg Function to aggregate scores (default: mean).
#' @param minchar Minimum number of characters for words.
#' @param word_size Numeric vector specifying range of word sizes.
#' @param top_words Number of top words to show.
#' @param palreverse Whether to reverse the palette colors.
#'
#' @return A ggplot object
#' @keywords internal
#' @importFrom rlang sym syms := "%||%"
#' @importFrom dplyr filter mutate group_by summarise reframe slice_max distinct n
#' @importFrom tidyr unnest
#' @importFrom ggplot2 ggplot aes scale_color_gradientn scale_size labs
#' @importFrom ggplot2 coord_flip guide_colorbar guide_legend guides theme
#' @importFrom ggwordcloud geom_text_wordcloud
#' @importFrom scales rescale
WordCloudPlotAtomic <- function(
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
    facet_by = NULL, facet_scales = "fixed",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Spectral", palcolor = NULL, alpha = 1, palreverse = FALSE,
    aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL,
    seed = 8525,
    ...) {
  # Get ggplot function (with gglogger support)
  ggplot <- get_ggplot()

  # Validate inputs
  if (sum(is.null(word_by), is.null(sentence_by)) != 1) {
    stop("Either 'word_by' or 'sentence_by' should be specified.", call. = FALSE)
  }
  if (!is.null(sentence_by) && !is.null(count_by)) {
    stop("Cannot specify 'count_by' when 'sentence_by' is specified.", call. = FALSE)
  }

  # Validate columns
  facet_by <- validate_columns(
    data, facet_by,
    force_factor = TRUE,
    allow_multi = TRUE
  )
  count_by <- validate_columns(data, count_by)
  score_by <- validate_columns(data, score_by)

  # Set defaults
  if (is.null(score_by)) {
    data$.score <- 1
    score_by <- ".score"
  }
  if (is.null(count_by)) {
    data$.count <- 1
    count_by <- ".count"
  }

  # Process sentences or words
  if (!is.null(sentence_by)) {
    sentence_by <- validate_columns(data, sentence_by)
    data <- data |>
      dplyr::mutate(
        word = strsplit(
          tolower(gsub("[[:punct:]]", "", as.character(!!rlang::sym(sentence_by)))),
          "\\b\\s+\\b"
        )
      ) |>
      tidyr::unnest(cols = "word")

    if (length(facet_by) == 1) {
      data <- data |>
        dplyr::group_by(!!!rlang::syms(c("word", facet_by))) |>
        dplyr::reframe(
          word = !!rlang::sym("word"),
          !!rlang::sym(facet_by) := !!rlang::sym(facet_by),
          count = sum(!!rlang::sym(count_by)),
          score = score_agg(!!rlang::sym(score_by)),
          .groups = "keep"
        )
    } else if (length(facet_by) == 2) {
      data <- data |>
        dplyr::group_by(!!!rlang::syms(c("word", facet_by))) |>
        dplyr::reframe(
          word = !!rlang::sym("word"),
          !!rlang::sym(facet_by[1]) := !!rlang::sym(facet_by[1]),
          !!rlang::sym(facet_by[2]) := !!rlang::sym(facet_by[2]),
          count = sum(!!rlang::sym(count_by)),
          score = score_agg(!!rlang::sym(score_by)),
          .groups = "keep"
        )
    } else {
      data <- data |>
        dplyr::group_by(!!rlang::sym("word")) |>
        dplyr::reframe(
          word = !!rlang::sym("word"),
          count = sum(!!rlang::sym(count_by)),
          score = score_agg(!!rlang::sym(score_by)),
          .groups = "keep"
        )
    }
  } else {
    word_by <- validate_columns(data, word_by)
    data <- data |> tidyr::unnest(cols = word_by)

    if (length(facet_by) == 1) {
      data <- data |>
        dplyr::group_by(!!!rlang::syms(unique(c(word_by, facet_by)))) |>
        dplyr::reframe(
          word = !!rlang::sym(word_by),
          !!rlang::sym(facet_by) := !!rlang::sym(facet_by),
          count = sum(!!rlang::sym(count_by)),
          score = score_agg(!!rlang::sym(score_by)),
          .groups = "keep"
        )
    } else if (length(facet_by) == 2) {
      data <- data |>
        dplyr::group_by(!!!rlang::syms(unique(c(word_by, facet_by)))) |>
        dplyr::reframe(
          word = !!rlang::sym(word_by),
          !!rlang::sym(facet_by[1]) := !!rlang::sym(facet_by[1]),
          !!rlang::sym(facet_by[2]) := !!rlang::sym(facet_by[2]),
          count = sum(!!rlang::sym(count_by)),
          score = score_agg(!!rlang::sym(score_by)),
          .groups = "keep"
        )
    } else {
      data <- data |>
        dplyr::group_by(!!rlang::sym(word_by)) |>
        dplyr::reframe(
          word = !!rlang::sym(word_by),
          count = sum(!!rlang::sym(count_by)),
          score = score_agg(!!rlang::sym(score_by)),
          .groups = "keep"
        )
    }
  }

  # Filter and process data
  data <- data |>
    dplyr::filter(!grepl(pattern = "\\[.*\\]", x = !!rlang::sym("word"))) |>
    dplyr::filter(nchar(!!rlang::sym("word")) >= minchar) |>
    dplyr::filter(!tolower(!!rlang::sym("word")) %in% tolower(words_excluded)) |>
    dplyr::distinct() |>
    dplyr::slice_max(order_by = !!rlang::sym("score"), n = top_words) |>
    dplyr::mutate(
      angle = 90 * sample(c(0, 1), dplyr::n(), replace = TRUE, prob = c(60, 40))
    ) |>
    as.data.frame()

  # Get colors
  colors <- get_palette(
    data$score,
    type = "continuous",
    palette = palette,
    palcolor = palcolor,
    matched = FALSE,
    reverse = palreverse
  )
  colors_value <- seq(
    min(data$score, na.rm = TRUE),
    stats::quantile(data$score, 0.99, na.rm = TRUE) + 0.001,
    length.out = 100
  )

  # Build plot
  p <- ggplot(
    data,
    ggplot2::aes(
      label = !!rlang::sym("word"),
      size = !!rlang::sym("count"),
      color = !!rlang::sym("score"),
      angle = !!rlang::sym("angle")
    )
  ) +
    ggwordcloud::geom_text_wordcloud(
      rm_outside = TRUE,
      eccentricity = 1,
      shape = "square",
      show.legend = TRUE,
      grid_margin = 3
    ) +
    ggplot2::scale_color_gradientn(
      name = score_name %||% "Score",
      colours = colors,
      values = scales::rescale(colors_value),
      guide = ggplot2::guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black",
        frame.linewidth = 0.3,
        ticks.linewidth = 0.3,
        title.hjust = 0
      )
    ) +
    ggplot2::scale_size(
      name = count_name %||% "Count",
      range = word_size,
      breaks = ceiling(seq(
        min(data$count, na.rm = TRUE),
        max(data$count, na.rm = TRUE),
        length.out = 3
      ))
    ) +
    ggplot2::guides(
      size = ggplot2::guide_legend(
        override.aes = list(colour = "black", label = "G"),
        order = 1
      )
    ) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::coord_flip()

  # Apply theme
  p <- p + do.call(theme, theme_args)

  # Apply styling based on data types
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = "word",
    y_var = NULL,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction,
    aspect.ratio = aspect.ratio
  )

  # Calculate plot dimensions
  height <- 4.5
  width <- 4.5
  if (legend.position %in% c("right", "left")) {
    width <- width + 1
  } else if (legend.direction == "horizontal") {
    height <- height + 1
  } else {
    height <- height + 2
  }

  # Set dimensions as attributes
  attr(p, "height") <- height
  attr(p, "width") <- width

  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- add_facets(
      p, facet_by, facet_scales,
      facet_nrow, facet_ncol, facet_byrow
    )
  }

  return(p)
}


#' Word Cloud Plot
#'
#' @description
#' Creates word cloud plots to illustrate word count/frequency.
#' Words can be sized by count and colored by score.
#' Supports splitting by groups, faceting, and custom color palettes.
#'
#' @inheritParams parameters
#' @inheritParams WordCloudPlotAtomic
#'
#' @return A ggplot object, patchwork object (if combine=TRUE), or list of plots
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample data with words
#' data <- data.frame(
#'   word = rep(c(
#'     "apple", "banana", "cherry", "date", "elderberry",
#'     "fig", "grape", "kiwi", "lemon", "mango"
#'   ), each = 5),
#'   count = sample(1:50, 50, replace = TRUE),
#'   score = rnorm(50, mean = 5, sd = 2),
#'   group = rep(c("A", "B"), 25)
#' )
#'
#' # Basic word cloud from words
#' WordCloudPlot(data, word_by = "word", count_by = "count", score_by = "score")
#'
#' # Word cloud with custom palette
#' WordCloudPlot(data,
#'   word_by = "word", count_by = "count", score_by = "score",
#'   palette = "RdYlBu", palreverse = TRUE
#' )
#'
#' # Word cloud with split by group
#' WordCloudPlot(data,
#'   word_by = "word", count_by = "count", score_by = "score",
#'   split_by = "group", combine = TRUE
#' )
#'
#' # Create sample data with sentences
#' sentence_data <- data.frame(
#'   text = c(
#'     "Data science is an interdisciplinary field",
#'     "Machine learning uses statistical techniques",
#'     "Artificial intelligence is transforming industries",
#'     "Deep learning is a subset of machine learning"
#'   ),
#'   score = c(1, 2, 3, 4)
#' )
#'
#' # Word cloud from sentences (auto-splitting)
#' WordCloudPlot(sentence_data,
#'   sentence_by = "text",
#'   score_by = "score",
#'   top_words = 20
#' )
#' }
WordCloudPlot <- function(
    data,
    word_by = NULL,
    sentence_by = NULL,
    count_by = NULL,
    score_by = NULL,
    count_name = NULL,
    score_name = NULL,
    split_by = NULL, split_by_sep = "_",
    words_excluded = ggforge::words_excluded,
    score_agg = mean,
    minchar = 2,
    word_size = c(2, 8),
    top_words = 100,
    facet_by = NULL, facet_scales = "fixed",
    facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_ggforge", theme_args = list(),
    palette = "Spectral", palcolor = NULL, alpha = 1, palreverse = FALSE,
    aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
    ...) {
  # Validate common arguments
  validate_common_args(
    seed = seed,
    facet_by = facet_by,
    split_by = split_by,
    theme = theme,
    palette = palette,
    alpha = alpha,
    aspect.ratio = aspect.ratio,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Process theme
  theme <- process_theme(theme)

  # Validate split_by
  split_by <- validate_columns(
    data, split_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = split_by_sep
  )

  # Collect all parameters for passing to atomic function
  params <- as.list(environment())
  params$data <- NULL # Remove data from params

  # Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = WordCloudPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}
