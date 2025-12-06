#' Cox Proportional-Hazards Model Visualization
#'
#' @description
#' Create publication-ready Cox proportional-hazards model visualizations including
#' hazard ratio curves, forest plots (simple and detailed), with support for
#' parallel computation and multiple visualization types.
#'
#' @inheritParams parameters
#' @param data A data frame containing survival time, event status, and variables for Cox analysis.
#' @param time Column name for time variable (numeric).
#' @param event Column name for event status (1=event, 0=censored).
#' @param vars Column name(s) for variables to include in Cox regression. If NULL, uses `var`.
#' @param var Column name for single variable (used for curve plot or when vars is NULL).
#' @param plot_type Type of Cox plot: "curve" (hazard ratio curve), "forest" (simple forest plot),
#'   or "forest2" (detailed forest plot with HR and p-value columns).
#' @param scale Logical. Whether to standardize variables to z-scores before Cox regression.
#' @param nonExpression_ratio Numeric. Threshold ratio for filtering non-expressed genes.
#'   If the proportion of zero values exceeds this ratio, the variable will be filtered out.
#' @param parallel Logical. Whether to perform parallel computation for multiple variables.
#' @param n_cores Integer. Number of cores to use for parallel computation.
#'   Default is detectCores() - 6, minimum 1.
#' @param ribbon_color Color for confidence interval ribbon (curve plot).
#' @param ribbon_alpha Alpha transparency for ribbon (curve plot).
#' @param line_color Color for hazard ratio line (curve plot).
#' @param line_type Line type for hazard ratio curve.
#' @param line_width Line width for hazard ratio curve.
#' @param show_cindex Logical. Whether to show concordance index (Cindex) on plot.
#' @param text_size Size of annotation text.
#' @param text_digit Number of significant digits for text display.
#' @param text_face Font face for annotation text.
#' @param point_colors Vector of colors for points in forest plot.
#'   Default: c("#ED6355", "#118ab2", "grey") for Risky, Protective, NoSig.
#' @param point_size Size of points in forest plot.
#' @param point_border_width Border width of points.
#' @param point_border_size Size of point borders.
#' @param line_colors Colors for error bars in forest plot.
#' @param cutoff_vline_type Line type for cutoff vertical line (HR = 1).
#' @param cutoff_vline_width Line width for cutoff vertical line.
#' @param cutoff_vline_color Color for cutoff vertical line.
#' @param x_log10_scale Logical. Whether to use log10 scale for x-axis in forest plot.
#' @param text_colors Text colors in forest2 plot.
#' @param digits Number of significant digits for numeric display.
#' @param rel_width Relative widths of columns in forest2 plot.
#'   Default: c(0.8, 1.4, 1.2, 0.6) for Variable, HR plot, HR text, P-value.
#'
#' @return A ggplot object, patchwork object, or list of plots depending on settings
#' @export
#' @importFrom survival coxph Surv
#' @importFrom stats predict as.formula quantile complete.cases
#' @importFrom parallel detectCores
#' @importFrom ggplot2 waiver
#'
#' @examples
#' \donttest{
#' # Prepare example data
#' library(survival)
#' data(lung)
#' lung$status <- lung$status - 1 # Convert to 0/1
#'
#' # Single variable hazard ratio curve
#' CoxPlot(
#'   data = lung,
#'   time = "time",
#'   event = "status",
#'   var = "age",
#'   plot_type = "curve"
#' )
#'
#' # Multiple variables forest plot (simple)
#' CoxPlot(
#'   data = lung,
#'   time = "time",
#'   event = "status",
#'   vars = c("age", "ph.ecog", "ph.karno", "pat.karno"),
#'   plot_type = "forest"
#' )
#'
#' # Multiple variables forest plot (detailed)
#' CoxPlot(
#'   data = lung,
#'   time = "time",
#'   event = "status",
#'   vars = c("age", "ph.ecog", "ph.karno", "pat.karno"),
#'   plot_type = "forest2"
#' )
#' }
CoxPlot <- function(
    data,
    time = "time",
    event = "event",
    vars = NULL,
    var = NULL,
    plot_type = c("curve", "forest", "forest2"),
    scale = FALSE,
    nonExpression_ratio = 1,
    parallel = FALSE,
    n_cores = parallel::detectCores() - 6,
    # Curve plot parameters
    ribbon_color = "#EFA63A",
    ribbon_alpha = 0.6,
    line_color = "#3a6ea5",
    line_type = 1,
    line_width = 1,
    show_cindex = TRUE,
    text_size = 3.5,
    text_digit = 3,
    text_face = "bold.italic",
    # Forest plot parameters
    point_colors = c("#ED6355", "#118ab2", "grey"),
    point_size = 2,
    point_border_width = 0.5,
    point_border_size = 3,
    line_colors = point_colors,
    cutoff_vline_type = 2,
    cutoff_vline_width = 0.5,
    cutoff_vline_color = "grey30",
    x_log10_scale = FALSE,
    # Forest2 plot parameters
    text_colors = "black",
    digits = 3,
    rel_width = c(0.8, 1.4, 1.2, 0.6),
    # Standard ggforge parameters
    split_by = NULL,
    split_by_sep = "_",
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    aspect.ratio = NULL,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    legend.position = "bottom",
    legend.direction = "horizontal",
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
  # Check required packages
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for CoxPlot. Please install it.", call. = FALSE)
  }

  # Validate plot type
  plot_type <- match.arg(plot_type)

  # Handle var/vars logic
  if (is.null(vars) && is.null(var)) {
    stop("Either 'vars' or 'var' must be specified", call. = FALSE)
  }
  if (is.null(vars)) {
    vars <- var
  }

  # Validate plot type and variables
  if (plot_type == "curve" && length(vars) > 1) {
    warning("Curve plot only supports single variable. Using first variable: ", vars[1], call. = FALSE)
    vars <- vars[1]
  }

  # Validate parameters
  .validate_cox_params(
    n_cores = n_cores,
    digits = digits,
    rel_width = rel_width,
    nonExpression_ratio = nonExpression_ratio
  )

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

  # Validate columns
  time <- validate_columns(data, time)
  event <- validate_columns(data, event)
  vars <- validate_columns(data, vars, allow_multi = TRUE)
  split_by <- validate_columns(
    data, split_by,
    force_factor = TRUE,
    allow_multi = TRUE,
    concat_multi = TRUE,
    concat_sep = split_by_sep
  )

  # Collect all parameters
  params <- as.list(environment())
  params$data <- NULL

  # Build plot using standard workflow
  build_plot(
    data = data,
    atomic_fn = CoxPlotAtomic,
    params = params,
    split_by = split_by,
    facet_by = facet_by,
    combine = combine
  )
}

#' Cox Plot Atomic Function
#'
#' @description
#' Creates a single Cox plot without splitting
#'
#' @inheritParams CoxPlot
#' @keywords internal
#' @importFrom ggplot2 ggplot aes
#' @importFrom dplyr %>%
#' @importFrom rlang "%||%"
CoxPlotAtomic <- function(
    data,
    time,
    event,
    vars,
    var,
    plot_type,
    scale,
    nonExpression_ratio,
    parallel,
    n_cores,
    ribbon_color,
    ribbon_alpha,
    line_color,
    line_type,
    line_width,
    show_cindex,
    text_size,
    text_digit,
    text_face,
    point_colors,
    point_size,
    point_border_width,
    point_border_size,
    line_colors,
    cutoff_vline_type,
    cutoff_vline_width,
    cutoff_vline_color,
    x_log10_scale,
    text_colors,
    digits,
    rel_width,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    theme = "theme_ggforge",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    aspect.ratio = NULL,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    legend.position = "bottom",
    legend.direction = "horizontal",
    seed = 8525,
    ...) {
  # Get ggplot function
  ggplot <- get_ggplot()

  # Route to appropriate plotting function
  if (plot_type == "curve") {
    p <- .cox_curve_plot(
      data = data,
      var = vars[1],
      time = time,
      event = event,
      ribbon_color = ribbon_color,
      ribbon_alpha = ribbon_alpha,
      line_color = line_color,
      line_type = line_type,
      line_width = line_width,
      text_size = text_size,
      text_digit = text_digit,
      text_face = text_face,
      xlab = xlab %||% vars[1],
      ylab = ylab %||% "Relative HR",
      title = title,
      show_cindex = show_cindex,
      theme = theme,
      theme_args = theme_args,
      aspect.ratio = aspect.ratio
    )
  } else {
    # 统一计算 Cox 表（forest 和 forest2 共享）
    cox_table <- .get_cox_table(
      data = data,
      vars = vars,
      time = time,
      event = event,
      scale = scale,
      nonExpression_ratio = nonExpression_ratio,
      parallel = parallel,
      n_cores = n_cores
    )

    # 根据类型创建森林图
    if (plot_type == "forest") {
      p <- .cox_forest_plot(
        data = cox_table,
        point_colors = point_colors,
        point_size = point_size,
        point_border_width = point_border_width,
        point_border_size = point_border_size,
        line_colors = line_colors,
        line_type = line_type,
        line_width = line_width,
        cutoff_vline_type = cutoff_vline_type,
        cutoff_vline_width = cutoff_vline_width,
        cutoff_vline_color = cutoff_vline_color,
        x_log10_scale = x_log10_scale,
        xlab = xlab %||% "HR (95%CI)",
        title = title,
        subtitle = subtitle,
        legend.position = legend.position,
        legend.direction = legend.direction,
        theme = theme,
        theme_args = theme_args,
        aspect.ratio = aspect.ratio
      )
    } else { # forest2
      p <- .cox_forest2_plot(
        data = cox_table,
        point_colors = point_colors,
        point_size = point_size,
        point_border_width = point_border_width,
        point_border_size = point_border_size,
        line_colors = line_colors,
        line_type = line_type,
        line_width = line_width,
        cutoff_vline_type = cutoff_vline_type,
        cutoff_vline_width = cutoff_vline_width,
        cutoff_vline_color = cutoff_vline_color,
        x_log10_scale = x_log10_scale,
        text_colors = text_colors,
        digits = digits,
        rel_width = rel_width,
        theme = theme,
        theme_args = theme_args
      )
    }
  }

  # Add faceting if requested (forest2 doesn't support facet well)
  if (!is.null(facet_by) && plot_type != "forest2") {
    p <- add_facets(
      p, facet_by, facet_scales,
      facet_nrow, facet_ncol, facet_byrow
    )
  }

  return(p)
}

# =============================================================================
# Parameter Validation
# =============================================================================

#' Validate Cox-specific parameters
#' @keywords internal
.validate_cox_params <- function(n_cores, digits, rel_width, nonExpression_ratio) {
  # Validate n_cores
  if (n_cores < 1) {
    n_cores <- max(1, parallel::detectCores() - 6)
    message("n_cores was < 1, reset to ", n_cores)
  }

  # Validate digits
  if (digits < 1) {
    stop("'digits' must be at least 1", call. = FALSE)
  }

  # Validate rel_width
  if (length(rel_width) != 4) {
    stop("'rel_width' must have exactly 4 elements (Variable, HR plot, HR text, P-value)", call. = FALSE)
  }

  # Validate nonExpression_ratio
  if (nonExpression_ratio < 0 || nonExpression_ratio > 1) {
    stop("'nonExpression_ratio' must be between 0 and 1", call. = FALSE)
  }

  invisible(NULL)
}

# =============================================================================
# Cox Table Computation
# =============================================================================

#' Compute Cox Table
#'
#' @description
#' Internal function to compute Cox proportional-hazards model summary table
#' with optional parallel computation
#'
#' @keywords internal
#' @importFrom survival coxph Surv
#' @importFrom stats as.formula
#' @importFrom dplyr rename
.get_cox_table <- function(
    data,
    vars,
    time = "time",
    event = "event",
    scale = FALSE,
    nonExpression_ratio = 1,
    parallel = FALSE,
    n_cores = 1) {
  # Rename columns using dplyr
  data_copy <- data %>%
    dplyr::rename(time = !!time, event = !!event)

  # Filter variables by expression ratio (optimized with colMeans)
  vardata <- data_copy[, vars, drop = FALSE]
  ind <- colMeans(vardata == 0, na.rm = TRUE) <= nonExpression_ratio
  vars <- names(vardata)[ind]

  if (length(vars) == 0) {
    stop("No variables passed filtering threshold (nonExpression_ratio = ",
      nonExpression_ratio, ")",
      call. = FALSE
    )
  }

  # Scale variables if requested
  if (scale) {
    data_copy[, vars] <- scale(data_copy[, vars])
  }

  # Define function to fit Cox model
  fit_cox_model <- function(var_name) {
    tryCatch(
      {
        formula <- as.formula(paste("survival::Surv(time, event) ~", var_name))
        fit <- survival::coxph(formula, data = data_copy)
        fit_summary <- summary(fit)

        # Check for multiple coefficients (categorical variable with >2 levels)
        n_coef <- nrow(fit_summary$coefficients)

        if (n_coef > 1) {
          level_names <- rownames(fit_summary$coefficients)
          clean_names <- gsub(paste0("^", var_name), "", level_names)

          result <- data.frame(
            Feature = paste0(var_name, "_", clean_names),
            HR = fit_summary$coefficients[, 2],
            HR.95H = fit_summary$conf.int[, 4],
            HR.95L = fit_summary$conf.int[, 3],
            COX_Pval = fit_summary$coefficients[, 5],
            Cindex = rep(fit_summary$concordance[1], n_coef),
            Cindex_se = rep(fit_summary$concordance[2], n_coef),
            row.names = NULL,
            stringsAsFactors = FALSE
          )
        } else {
          result <- data.frame(
            Feature = var_name,
            HR = fit_summary$coefficients[, 2],
            HR.95H = fit_summary$conf.int[, 4],
            HR.95L = fit_summary$conf.int[, 3],
            COX_Pval = fit_summary$coefficients[, 5],
            Cindex = fit_summary$concordance[1],
            Cindex_se = fit_summary$concordance[2],
            row.names = NULL,
            stringsAsFactors = FALSE
          )
        }

        return(result)
      },
      error = function(e) {
        warning("Failed to fit Cox model for variable: ", var_name,
          "\n  Error: ", e$message,
          call. = FALSE
        )
        return(NULL)
      }
    )
  }

  # Perform computation (parallel or sequential)
  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("furrr", quietly = TRUE)) {
      warning("Packages 'future' and 'furrr' required for parallel computation. ",
        "Running sequentially.",
        call. = FALSE
      )
      parallel <- FALSE
    }
  }

  if (parallel) {
    future::plan(future::multisession, workers = n_cores)
    result <- furrr::future_map_dfr(vars, fit_cox_model)
  } else {
    result <- do.call(rbind, lapply(vars, fit_cox_model))
  }

  # Remove NULL results
  result <- result[!is.na(result$Feature), ]

  if (nrow(result) == 0) {
    stop("Failed to fit Cox models for any variables", call. = FALSE)
  }

  return(result)
}

# =============================================================================
# Shared Forest Plot Functions
# =============================================================================

#' Prepare forest plot data
#' @description Shared data preparation for both forest plot types
#' @keywords internal
.prepare_forest_data <- function(data, point_colors, digits = NULL) {
  # Order by HR
  data <- data[order(data$HR), ]
  data$Feature <- factor(data$Feature, levels = data$Feature)

  # Determine type based on P value and HR
  data$Type <- ifelse(data$COX_Pval > 0.05, "NoSig",
    ifelse(data$HR > 1, "Risky", "Protective")
  )
  data$Type <- factor(data$Type, levels = c("Risky", "Protective", "NoSig"))

  # Set color names
  names(point_colors) <- c("Risky", "Protective", "NoSig")

  # Add formatted text if digits provided (for forest2)
  if (!is.null(digits)) {
    data$HRS <- sprintf(
      "%.*f [%.*f-%.*f]",
      digits, data$HR,
      digits, data$HR.95L,
      digits, data$HR.95H
    )
    data$Plabel <- signif(data$COX_Pval, digits)
  }

  list(data = data, point_colors = point_colors)
}

#' Create base forest plot
#' @description Shared base plot creation for forest plots
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_vline geom_errorbar geom_point scale_color_manual
.create_forest_base <- function(
    data,
    point_colors,
    point_size,
    point_border_width,
    point_border_size,
    line_colors,
    line_type,
    line_width,
    cutoff_vline_type,
    cutoff_vline_width,
    cutoff_vline_color,
    x_log10_scale) {
  # Get x-axis scale
  x_scale <- if (x_log10_scale) {
    ggplot2::scale_x_log10()
  } else {
    ggplot2::scale_x_continuous()
  }

  # Create error bar geom
  error_bar <- if (all(point_colors == line_colors)) {
    ggplot2::geom_errorbar(
      ggplot2::aes(xmax = .data$HR.95H, xmin = .data$HR.95L, color = .data$Type),
      linetype = line_type,
      linewidth = line_width, width = 0
    )
  } else {
    ggplot2::geom_errorbar(
      ggplot2::aes(xmax = .data$HR.95H, xmin = .data$HR.95L),
      linetype = line_type,
      color = line_colors[1],
      linewidth = line_width, width = 0
    )
  }

  # Build base plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$HR, y = .data$Feature)) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = cutoff_vline_type,
      linewidth = cutoff_vline_width,
      color = cutoff_vline_color
    ) +
    error_bar +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$Type),
      shape = 15,
      size = point_size
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$Type),
      shape = 22,
      size = point_border_size,
      stroke = point_border_width
    ) +
    ggplot2::scale_color_manual(values = point_colors) +
    x_scale

  list(plot = p, x_scale = x_scale, error_bar = error_bar)
}

#' Create text geom for forest2
#' @description Helper to create text geoms with optional coloring
#' @keywords internal
#' @importFrom ggplot2 geom_text aes
.create_text_geom <- function(mapping, text_size, point_colors, text_colors, ...) {
  if (all(point_colors == text_colors)) {
    ggplot2::geom_text(mapping, size = text_size, show.legend = FALSE, ...)
  } else {
    ggplot2::geom_text(mapping, color = text_colors[1], size = text_size, show.legend = FALSE, ...)
  }
}

# =============================================================================
# Plot Functions
# =============================================================================

#' Cox Curve Plot
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_text labs
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous expansion
#' @importFrom survival coxph Surv
#' @importFrom stats predict as.formula complete.cases
#' @importFrom dplyr arrange mutate %>%
#' @importFrom rlang .data "%||%"
.cox_curve_plot <- function(
    data,
    var,
    time,
    event,
    ribbon_color,
    ribbon_alpha,
    line_color,
    line_type,
    line_width,
    text_size,
    text_digit,
    text_face,
    xlab,
    ylab,
    title,
    show_cindex,
    theme,
    theme_args,
    aspect.ratio) {
  # Rename columns
  data_clean <- data %>%
    dplyr::rename(time = !!time, event = !!event) %>%
    dplyr::filter(complete.cases(.[, c("time", "event", var)]))

  # Fit Cox model
  formula <- as.formula(paste("survival::Surv(time, event) ~", var))
  aCox <- survival::coxph(formula, data = data_clean)

  # Extract statistics
  fit_summary <- summary(aCox)
  p_val <- fit_summary$coefficients[, 5]
  coef <- fit_summary$coefficients[, 1]
  cindex <- fit_summary$concordance[1]

  # Predict risk
  aPred <- stats::predict(aCox, type = "risk", se.fit = TRUE)
  hr <- aPred$fit
  high <- hr + 2 * aPred$se.fit
  low <- hr - 2 * aPred$se.fit

  # Create result dataframe
  # Convert var to numeric for plotting (handle both numeric and factor)
  var_values <- data_clean[[var]]
  if (is.factor(var_values) || is.character(var_values)) {
    var_numeric <- as.numeric(as.factor(var_values))
    var_labels <- levels(as.factor(var_values))
  } else {
    var_numeric <- var_values
    var_labels <- NULL
  }
  
  dd <- data.frame(
    var_original = var_values,
    var = var_numeric,
    HR = hr,
    HR.95H = high,
    HR.95L = low
  ) %>%
    dplyr::arrange(.data$var) %>%
    dplyr::mutate(x = 1:length(.data$var))

  # Prepare label text
  lb <- if (show_cindex) {
    paste0(
      "HR = ", signif(exp(coef), text_digit), "\n",
      "P = ", signif(p_val, text_digit), "\n",
      "C = ", signif(cindex, text_digit)
    )
  } else {
    paste0(
      "HR = ", signif(exp(coef), text_digit), "\n",
      "P = ", signif(p_val, text_digit)
    )
  }

  # Determine text position (use first coefficient if multiple)
  coef_sign <- coef[1]
  text_data <- if (coef_sign > 0) {
    data.frame(var = min(dd$var), HR.95H = max(dd$HR.95H))
  } else {
    data.frame(var = max(dd$var), HR.95H = max(dd$HR.95H))
  }
  text_hjust <- if (coef_sign > 0) -0.1 else 1.1

  # Get base size
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")

  # Generate plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = dd,
      ggplot2::aes(x = .data$var, y = .data$HR, ymin = .data$HR.95L, ymax = .data$HR.95H),
      fill = ribbon_color, alpha = ribbon_alpha
    ) +
    ggplot2::geom_line(
      data = dd,
      ggplot2::aes(x = .data$var, y = .data$HR),
      linewidth = line_width, color = line_color, linetype = line_type
    ) +
    ggplot2::geom_text(
      data = text_data,
      ggplot2::aes(x = .data$var, y = .data$HR.95H),
      label = lb, size = text_size,
      vjust = 1, hjust = text_hjust,
      fontface = text_face, color = "black"
    ) +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0.01, 0.01))) +
    do.call(theme, theme_args)
  
  # Add appropriate x scale
  if (!is.null(var_labels)) {
    # Factor variable: use discrete scale with labels
    p <- p + ggplot2::scale_x_continuous(
      breaks = seq_along(var_labels),
      labels = var_labels,
      expand = ggplot2::expansion(c(0, 0))
    )
  } else {
    # Numeric variable: use continuous scale
    p <- p + ggplot2::scale_x_continuous(expand = ggplot2::expansion(c(0, 0)))
  }

  # Apply styling
  p <- apply_style_theme(
    plot = p,
    data = dd,
    x_var = "var",
    y_var = "HR",
    flip = FALSE,
    base_size = base_size,
    legend.position = "none"
  )

  # Add custom theme adjustments
  if (!is.null(aspect.ratio)) {
    p <- p + ggplot2::theme(aspect.ratio = aspect.ratio)
  }

  return(p)
}

#' Cox Forest Plot (Simple)
#' @keywords internal
#' @importFrom ggplot2 labs theme
#' @importFrom rlang "%||%" .data
.cox_forest_plot <- function(
    data,
    point_colors,
    point_size,
    point_border_width,
    point_border_size,
    line_colors,
    line_type,
    line_width,
    cutoff_vline_type,
    cutoff_vline_width,
    cutoff_vline_color,
    x_log10_scale,
    xlab,
    title,
    subtitle,
    legend.position,
    legend.direction,
    theme,
    theme_args,
    aspect.ratio) {
  # Prepare data (shared function)
  prepared <- .prepare_forest_data(data, point_colors)
  data <- prepared$data
  point_colors <- prepared$point_colors

  # Get base size
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")

  # Create base forest plot (shared function)
  base_plot <- .create_forest_base(
    data, point_colors, point_size, point_border_width, point_border_size,
    line_colors, line_type, line_width,
    cutoff_vline_type, cutoff_vline_width, cutoff_vline_color,
    x_log10_scale
  )

  # Add labels and theme
  p <- base_plot$plot +
    ggplot2::labs(x = xlab, y = NULL, color = NULL, title = title, subtitle = subtitle) +
    do.call(theme, theme_args)

  # Apply styling
  p <- apply_style_theme(
    plot = p,
    data = data,
    x_var = "HR",
    y_var = "Feature",
    flip = FALSE,
    base_size = base_size,
    legend.position = legend.position,
    legend.direction = legend.direction
  )

  # Add custom theme adjustments
  if (!is.null(aspect.ratio)) {
    p <- p + ggplot2::theme(aspect.ratio = aspect.ratio)
  }

  return(p)
}

#' Cox Forest Plot (Detailed)
#' @keywords internal
#' @importFrom ggplot2 ggplot aes scale_y_continuous coord_flip theme_void
#' @importFrom ggplot2 labs theme element_blank element_text margin
#' @importFrom patchwork plot_layout
#' @importFrom rlang "%||%" .data
.cox_forest2_plot <- function(
    data,
    point_colors,
    point_size,
    point_border_width,
    point_border_size,
    line_colors,
    line_type,
    line_width,
    cutoff_vline_type,
    cutoff_vline_width,
    cutoff_vline_color,
    x_log10_scale,
    text_colors,
    digits,
    rel_width,
    theme,
    theme_args) {
  # Prepare data (shared function, with formatted text)
  prepared <- .prepare_forest_data(data, point_colors, digits = digits)
  data <- prepared$data
  point_colors <- prepared$point_colors

  # Get base size
  base_size <- theme_args$base_size %||% ggforge_option("theme.base_size")
  text_size_scale <- base_size / 12
  text_size <- 4 * text_size_scale

  # Plot 1: Variable names
  p1 <- ggplot2::ggplot(data) +
    .create_text_geom(
      ggplot2::aes(x = .data$Feature, y = 0, label = .data$Feature, color = .data$Type),
      text_size, point_colors, text_colors,
      hjust = 0
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 0.02)) +
    ggplot2::coord_flip() +
    ggplot2::theme_void() +
    ggplot2::labs(x = NULL, y = NULL, title = "Variable") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5.5, 0, 5.5, 5.5),
      plot.title = build_element_text("font.title", base_size, hjust = 0, vjust = 2)
    )

  # Plot 2: HR and confidence intervals (use shared base function)
  base_plot <- .create_forest_base(
    data, point_colors, point_size, point_border_width, point_border_size,
    line_colors, line_type, line_width,
    cutoff_vline_type, cutoff_vline_width, cutoff_vline_color,
    x_log10_scale
  )

  p2 <- base_plot$plot +
    ggplot2::labs(x = NULL, y = NULL, color = NULL, title = "Hazard ratio") +
    do.call(theme, theme_args) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = build_element_text("font.axis_text.continuous", base_size),
      axis.title = build_element_text("font.axis_title", base_size),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5.5, 0, 5.5, 0),
      plot.title = build_element_text("font.title", base_size, hjust = 0.5, vjust = 0),
      legend.position = "none",
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank()
    )

  # Plot 3: HR text
  p3 <- ggplot2::ggplot(data) +
    .create_text_geom(
      ggplot2::aes(x = .data$Feature, y = 0.01, label = .data$HRS, color = .data$Type),
      text_size, point_colors, text_colors
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 0.02)) +
    ggplot2::coord_flip() +
    ggplot2::theme_void() +
    ggplot2::labs(x = NULL, y = NULL, title = "HR (95%CI)") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5.5, 0, 5.5, 0),
      plot.title = build_element_text("font.title", base_size, hjust = 0.5, vjust = 2)
    )

  # Plot 4: P-value
  p4 <- ggplot2::ggplot(data) +
    .create_text_geom(
      ggplot2::aes(x = .data$Feature, y = 0.01, label = .data$Plabel, color = .data$Type),
      text_size, point_colors, text_colors
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 0.02)) +
    ggplot2::coord_flip() +
    ggplot2::theme_void() +
    ggplot2::labs(x = NULL, y = NULL, title = "Pval") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 0),
      plot.title = build_element_text("font.title", base_size, hjust = 0.5, vjust = 2)
    )

  # Combine plots
  return(p1 + p2 + p3 + p4 + patchwork::plot_layout(nrow = 1, widths = rel_width))
}
