#' ggforge Global Configuration
#'
#' @description
#' Configuration system for ggforge package. This provides a centralized
#' way to manage default settings and options.
#'
#' @name config
#' @keywords internal
NULL

#' Get or set ggforge options
#'
#' @param ... Named arguments to set options, or character strings to get options
#' @return If setting options, returns invisible NULL. If getting options, returns the option value.
#' @keywords internal
#' @examples
#' \dontrun{
#' # Set options
#' ggforge_option(theme.base_size = 14, theme.font_family = "Arial")
#'
#' # Get options
#' ggforge_option("theme.base_size")
#' }
ggforge_option <- function(...) {
  args <- list(...)

  if (length(args) == 0) {
    # Return all ggforge options
    opts <- options()
    opts[grepl("^ggforge\\.", names(opts))]
  } else if (length(args) == 1 && is.null(names(args))) {
    # Get single option
    opt_name <- paste0("ggforge.", args[[1]])
    getOption(opt_name, default = .ggforge_defaults[[args[[1]]]])
  } else {
    # Set options
    opt_names <- paste0("ggforge.", names(args))
    names(args) <- opt_names
    do.call(options, args)
    invisible(NULL)
  }
}

#' Default configuration values
#' @keywords internal
.ggforge_defaults <- list(
  theme.base_size = 12,
  theme.font_family = NULL,
  theme.default = "theme_ggforge",
  palette.default = "Paired",
  seed = 8525,
  gglogger.enabled = FALSE
)

#' Initialize ggforge options on package load
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Set default options if not already set
  for (opt_name in names(.ggforge_defaults)) {
    full_opt_name <- paste0("ggforge.", opt_name)
    if (is.null(getOption(full_opt_name))) {
      opt_list <- list(.ggforge_defaults[[opt_name]])
      names(opt_list) <- full_opt_name
      do.call(options, opt_list)
    }
  }
}
