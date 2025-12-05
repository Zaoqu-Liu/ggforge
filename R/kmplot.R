#' Kaplan-Meier Survival Plot Atomic
#'
#' @description
#' Creates a single Kaplan-Meier survival plot without splitting.
#' This is the core plotting function that handles the actual ggplot construction.
#'
#' @inheritParams parameters
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_step geom_ribbon scale_color_manual scale_fill_manual
#' @importFrom ggplot2 labs annotate coord_cartesian geom_hline geom_vline scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous expansion geom_text geom_segment
#' @importFrom dplyr group_by summarise ungroup arrange filter mutate .data
#' @importFrom rlang sym syms := "%||%"
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom survival survfit Surv survdiff
#' @importFrom stats pchisq
KMPlotAtomic <- function(
    data,
    time,
    status,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    show_pval = TRUE,
    pval_method = "logrank",
    pval_digits = 4,
    pval_size = 4.5,
    pval_coord = c(0.05, 0.1),
    show_conf_int = FALSE,
    conf_alpha = 0.2,
    show_median_line = "none",
    median_linetype = 2,
    median_linewidth = 0.6,
    line_width = 1.3,
    show_risk_table = FALSE,
    risk_table_height = 0.25,
    risk_table_fontsize = 3.5,
    show_censors = TRUE,
    censor_shape = 3,
    censor_size = 4,
    censor_stroke = 0.5,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    aspect.ratio = NULL,
    x_breaks = NULL,
    y_breaks = waiver(),
    x_min = NULL,
    x_max = NULL,
    y_min = 0,
    y_max = 1,
    legend.position = "top",
    legend.direction = "horizontal",
    title = NULL,
    subtitle = NULL,
    xlab = "Time",
    ylab = "Survival Probability",
    facet_by = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    ...) {
  # Get ggplot function (with gglogger support)
  ggplot <- get_ggplot()

  # Validate columns
  time <- validate_columns(data, time)
  status <- validate_columns(data, status)
  group_by <- validate_columns(
    data, group_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = group_by_sep
  )

  # Handle NULL group_by (single curve)
  has_groups <- !is.null(group_by)
  if (!has_groups) {
    group_by <- ".group"
    data[[group_by]] <- factor("All")
  }

  # Build survival fit
  if (has_groups && length(unique(data[[group_by]])) > 1) {
    formula_str <- sprintf("Surv(%s, %s) ~ %s", time, status, group_by)
  } else {
    formula_str <- sprintf("Surv(%s, %s) ~ 1", time, status)
  }
  sfit <- survival::survfit(as.formula(formula_str), data = data)

  # Extract survival data
  if (has_groups && length(unique(data[[group_by]])) > 1) {
    surv_data <- data.frame(
      time = sfit$time,
      n.risk = sfit$n.risk,
      n.event = sfit$n.event,
      n.censor = sfit$n.censor,
      surv = sfit$surv,
      std.err = sfit$std.err,
      lower = sfit$lower,
      upper = sfit$upper,
      strata = rep(names(sfit$strata), sfit$strata),
      stringsAsFactors = FALSE
    )
    surv_data[[group_by]] <- sub(".*=", "", surv_data$strata)
    surv_data[[group_by]] <- factor(surv_data[[group_by]], levels = sub(".*=", "", names(sfit$strata)))
  } else {
    surv_data <- data.frame(
      time = sfit$time,
      n.risk = sfit$n.risk,
      n.event = sfit$n.event,
      n.censor = sfit$n.censor,
      surv = sfit$surv,
      std.err = sfit$std.err,
      lower = sfit$lower,
      upper = sfit$upper,
      stringsAsFactors = FALSE
    )
    surv_data[[group_by]] <- factor("All")
  }

  # Get group levels
  group_levels <- levels(surv_data[[group_by]])
  n_groups <- length(group_levels)

  # Add time=0 point for step plot
  time0_list <- lapply(group_levels, function(grp) {
    data.frame(
      time = 0,
      n.risk = max(surv_data$n.risk[surv_data[[group_by]] == grp], na.rm = TRUE),
      n.event = 0,
      n.censor = 0,
      surv = 1,
      std.err = 0,
      lower = 1,
      upper = 1,
      stringsAsFactors = FALSE
    ) -> df
    df[[group_by]] <- factor(grp, levels = group_levels)
    if (has_groups && length(unique(data[[group_by]])) > 1) {
      df$strata <- names(sfit$strata)[which(group_levels == grp)]
    }
    df
  })
  time0 <- do.call(rbind, time0_list)

  surv_data <- rbind(time0, surv_data)
  surv_data <- surv_data %>% dplyr::arrange(!!sym(group_by), .data$time)

  # Setup colors
  colors <- get_palette(group_levels, palette = palette, palcolor = palcolor)

  # Calculate axis limits
  if (is.null(x_min)) x_min <- 0
  if (is.null(x_max)) x_max <- max(surv_data$time, na.rm = TRUE) * 1.05

  # Calculate x-axis breaks
  if (is.null(x_breaks)) {
    x_breaks <- pretty(c(x_min, x_max), n = 6)
    x_breaks <- x_breaks[x_breaks >= x_min & x_breaks <= x_max]
  }

  # Calculate p-value if requested
  pval_text <- NULL
  if (show_pval && has_groups && n_groups > 1) {
    tryCatch(
      {
        sdiff <- survival::survdiff(as.formula(formula_str), data = data)
        pval <- 1 - stats::pchisq(sdiff$chisq, length(sdiff$n) - 1)
        if (pval < 10^(-pval_digits)) {
          pval_text <- sprintf("p < %s", format(10^(-pval_digits), scientific = FALSE))
        } else {
          pval_text <- sprintf("p = %s", format(pval, digits = pval_digits, scientific = FALSE))
        }
      },
      error = function(e) {
        warning("Failed to calculate p-value: ", e$message, call. = FALSE)
      }
    )
  }

  # Build main survival plot
  p <- ggplot(surv_data, aes(x = .data$time, y = .data$surv, color = !!sym(group_by)))

  # Add confidence interval ribbons
  if (show_conf_int) {
    p <- p + ggplot2::geom_ribbon(
      aes(ymin = .data$lower, ymax = .data$upper, fill = !!sym(group_by)),
      alpha = conf_alpha,
      linetype = 0,
      show.legend = FALSE
    )
  }

  # Add survival curves
  p <- p + ggplot2::geom_step(linewidth = line_width, alpha = alpha)

  # Add censoring marks
  if (show_censors) {
    censor_data <- surv_data[surv_data$n.censor > 0, ]
    if (nrow(censor_data) > 0) {
      p <- p + ggplot2::geom_point(
        data = censor_data,
        aes(x = .data$time, y = .data$surv, color = !!sym(group_by)),
        shape = censor_shape,
        size = censor_size,
        stroke = censor_stroke,
        show.legend = FALSE
      )
    }
  }

  # Add median survival lines
  if (show_median_line %in% c("h", "v", "hv")) {
    # Calculate median survival times
    median_surv <- summary(sfit)$table
    if (is.matrix(median_surv)) {
      median_times <- median_surv[, "median"]
      names(median_times) <- sub(".*=", "", rownames(median_surv))
    } else {
      median_times <- median_surv["median"]
      names(median_times) <- "All"
    }

    if (show_median_line %in% c("h", "hv")) {
      p <- p + ggplot2::geom_hline(
        yintercept = 0.5,
        linetype = median_linetype,
        linewidth = median_linewidth,
        color = "grey50"
      )
    }

    if (show_median_line %in% c("v", "hv")) {
      for (i in seq_along(median_times)) {
        if (!is.na(median_times[i])) {
          p <- p + ggplot2::geom_vline(
            xintercept = median_times[i],
            linetype = median_linetype,
            linewidth = median_linewidth,
            color = colors[i]
          )
        }
      }
    }
  }

  # Add p-value annotation
  if (!is.null(pval_text)) {
    p <- p + ggplot2::annotate(
      "text",
      label = pval_text,
      x = x_max * pval_coord[1],
      y = pval_coord[2],
      hjust = 0,
      vjust = 0,
      size = pval_size
    )
  }

  # Setup legend labels
  if (has_groups) {
    legend_labels <- paste0(group_levels, " (n=", summary(sfit)$n, ")")
    names(legend_labels) <- group_levels
  } else {
    legend_labels <- paste0("All (n=", summary(sfit)$n, ")")
    names(legend_labels) <- "All"
  }

  # Add color and fill scales
  p <- p +
    ggplot2::scale_color_manual(
      name = group_name %||% group_by,
      values = colors,
      labels = legend_labels
    ) +
    ggplot2::scale_fill_manual(
      name = group_name %||% group_by,
      values = colors,
      labels = legend_labels,
      guide = "none"
    )

  # Add scales
  p <- p +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      limits = c(x_min, x_max),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      limits = c(y_min, y_max),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    )

  # Add labels
  p <- p + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    x = xlab,
    y = ylab
  )

  # Apply theme
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  text_size_scale <- base_size / 12 # Define once for both main plot and risk table
  p <- p + do.call(theme, theme_args)

  # Apply data-driven styling
  p <- apply_style_theme(
    plot = p,
    data = surv_data,
    x_var = time,
    y_var = "surv",
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Custom theme adjustments
  if (!is.null(aspect.ratio)) {
    p <- p + ggplot2::theme(aspect.ratio = aspect.ratio)
  }

  # Add margin for risk table
  if (show_risk_table) {
    p <- p + ggplot2::theme(
      plot.margin = ggplot2::margin(5.5, 5.5, 0, 5.5)
    )
  }

  # Build risk table if requested
  if (show_risk_table) {
    # Create risk table data
    risk_data <- data.frame()
    for (grp in group_levels) {
      grp_data <- surv_data[surv_data[[group_by]] == grp, ]
      n_at_risk <- sapply(x_breaks, function(t) {
        idx <- which(grp_data$time <= t)
        if (length(idx) > 0) {
          grp_data$n.risk[max(idx)]
        } else {
          0
        }
      })
      risk_data <- rbind(risk_data, data.frame(
        time = x_breaks,
        n.risk = n_at_risk,
        group = grp,
        stringsAsFactors = FALSE
      ))
    }
    names(risk_data)[3] <- group_by
    risk_data[[group_by]] <- factor(risk_data[[group_by]], levels = rev(group_levels))

    # Build risk table plot
    p_risk <- ggplot(risk_data, aes(x = .data$time, y = !!sym(group_by))) +
      ggplot2::geom_text(
        aes(label = .data$n.risk),
        size = risk_table_fontsize * text_size_scale,
        vjust = 0.5,
        color = "black"
      ) +
      ggplot2::scale_x_continuous(
        breaks = x_breaks,
        limits = c(x_min, x_max),
        expand = ggplot2::expansion(mult = c(0.02, 0.02))
      ) +
      ggplot2::labs(
        x = xlab,
        y = "At risk",
        title = "Number at risk"
      ) +
      do.call(theme, theme_args)

    # Apply same styling as main plot
    p_risk <- apply_style_theme(
      plot = p_risk,
      data = risk_data,
      x_var = "time",
      y_var = group_by,
      flip = FALSE,
      base_size = base_size,
      legend.position = "none"
    )

    # Additional risk table specific styling
    p_risk <- p_risk + ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        color = "black",
        face = "bold"
      ),
      axis.text.x = ggplot2::element_text(
        color = "black"
      ),
      axis.title = ggplot2::element_text(
        color = "black",
        face = "bold"
      ),
      plot.title = ggplot2::element_text(
        color = "black",
        face = "bold",
        hjust = 0
      ),
      plot.margin = ggplot2::margin(0, 5.5, 5.5, 5.5)
    )

    # Remove x-axis title from main plot
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())

    # Combine plots
    p_combined <- p / p_risk +
      patchwork::plot_layout(
        ncol = 1,
        heights = c(1 - risk_table_height, risk_table_height)
      )

    return(p_combined)
  }

  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- add_facets(
      p, facet_by, facet_scales,
      facet_nrow, facet_ncol, facet_byrow
    )
  }

  return(p)
}

#' Kaplan-Meier Survival Plot
#'
#' @description
#' Create publication-ready Kaplan-Meier survival curves with optional risk tables,
#' confidence intervals, and statistical comparisons.
#'
#' This function provides a complete implementation of Kaplan-Meier survival analysis
#' visualization, supporting single or multiple groups, with automatic p-value
#' calculation using the log-rank test.
#'
#' @inheritParams parameters
#' @param time Column name for time variable (numeric).
#' @param status Column name for event status (1=event, 0=censored).
#' @param group_by Column(s) for grouping survival curves.
#' @param group_by_sep Separator for concatenating multiple group columns.
#' @param group_name Legend title for groups.
#' @param show_pval Show log-rank test p-value.
#' @param pval_method P-value calculation method ("logrank").
#' @param pval_digits Number of digits for p-value.
#' @param pval_size Text size for p-value.
#' @param pval_coord Position of p-value as c(x, y) where x is fraction of x-axis, y is absolute.
#' @param show_conf_int Show confidence interval ribbons.
#' @param conf_alpha Transparency for confidence interval ribbons.
#' @param show_median_line Show median survival lines: "none", "h", "v", "hv".
#' @param median_linetype Line type for median survival lines.
#' @param median_linewidth Line width for median survival lines.
#' @param line_width Width of survival curves.
#' @param show_risk_table Show risk table below plot.
#' @param risk_table_height Relative height of risk table (0-1).
#' @param risk_table_fontsize Font size for numbers in risk table.
#' @param show_censors Show censoring marks on curves.
#' @param censor_shape Shape for censoring marks.
#' @param censor_size Size for censoring marks.
#' @param censor_stroke Stroke width for censoring marks.
#' @param x_breaks Custom x-axis breaks (time points).
#' @param y_breaks Custom y-axis breaks.
#' @param x_min Minimum x-axis value.
#' @param x_max Maximum x-axis value.
#' @param y_min Minimum y-axis value (default: 0).
#' @param y_max Maximum y-axis value (default: 1).
#'
#' @return A ggplot object, list of plots, or combined plots
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' \donttest{
#' library(survival)
#'
#' # Basic Kaplan-Meier plot
#' KMPlot(data = lung, time = "time", status = "status")
#'
#' # Multiple groups with p-value
#' KMPlot(
#'   data = lung,
#'   time = "time",
#'   status = "status",
#'   group_by = "sex",
#'   show_pval = TRUE
#' )
#'
#' # With risk table
#' KMPlot(
#'   data = lung,
#'   time = "time",
#'   status = "status",
#'   group_by = "sex",
#'   show_risk_table = TRUE,
#'   show_pval = TRUE
#' )
#'
#' # With confidence intervals and median lines
#' KMPlot(
#'   data = lung,
#'   time = "time",
#'   status = "status",
#'   group_by = "sex",
#'   show_conf_int = TRUE,
#'   show_median_line = "hv",
#'   palette = "Set1"
#' )
#'
#' # Publication-ready plot
#' KMPlot(
#'   data = lung,
#'   time = "time",
#'   status = "status",
#'   group_by = "sex",
#'   show_risk_table = TRUE,
#'   show_pval = TRUE,
#'   show_conf_int = TRUE,
#'   show_median_line = "hv",
#'   palette = "jco",
#'   title = "Overall Survival by Sex",
#'   xlab = "Time (days)",
#'   ylab = "Survival Probability",
#'   theme_args = list(base_size = 14)
#' )
#' }
KMPlot <- function(
    data,
    time,
    status,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    split_by = NULL,
    split_by_sep = "_",
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    show_pval = TRUE,
    pval_method = "logrank",
    pval_digits = 4,
    pval_size = 4.5,
    pval_coord = c(0.05, 0.1),
    show_conf_int = FALSE,
    conf_alpha = 0.2,
    show_median_line = "none",
    median_linetype = 2,
    median_linewidth = 0.6,
    line_width = 1.3,
    show_risk_table = FALSE,
    risk_table_height = 0.25,
    risk_table_fontsize = 3.5,
    show_censors = TRUE,
    censor_shape = 3,
    censor_size = 4,
    censor_stroke = 0.5,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    aspect.ratio = NULL,
    x_breaks = NULL,
    y_breaks = waiver(),
    x_min = NULL,
    x_max = NULL,
    y_min = 0,
    y_max = 1,
    legend.position = "top",
    legend.direction = "horizontal",
    title = NULL,
    subtitle = NULL,
    xlab = "Time",
    ylab = "Survival Probability",
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    seed = 8525,
    axes = NULL,
    axis_titles = axes,
    guides = NULL,
    design = NULL,
    ...) {
  # Check if survival package is available
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for KMPlot. Please install it.", call. = FALSE)
  }

  # Validate median line option
  show_median_line <- match.arg(show_median_line, c("none", "h", "v", "hv"))

  # Validate common arguments
  validate_common_args(
    seed = seed,
    facet_by = facet_by,
    split_by = split_by,
    theme = theme,
    palette = palette,
    alpha = alpha,
    aspect.ratio = aspect.ratio,
    legend.position = if (is.character(legend.position)) legend.position else "top",
    legend.direction = legend.direction
  )

  # Process theme
  theme <- process_theme(theme)

  # Validate split_by column
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
    atomic_fn = KMPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}
