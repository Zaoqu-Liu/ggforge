#' Color Palette Collection
#'
#' @description
#' A comprehensive collection of color palettes for data visualization.
#' Includes palettes from RColorBrewer, ggsci, viridis, and custom palettes.
#'
#' @format A named list of color vectors. Each palette has an attribute "type"
#'   indicating whether it is "discrete" or "continuous".
#'
#' @seealso \code{\link{get_palette}}, \code{\link{show_palettes}}
#'
#' @name palette_list
#' @docType data
#' @keywords datasets
"palette_list"

#' Example Enrichment Analysis Data
#'
#' @description
#' Example data from clusterProfiler enrichment analysis.
#' Used for demonstrating enrichment plot functions.
#'
#' @format A data frame with enrichment results
#'
#' @name enrich_example
#' @docType data
#' @keywords datasets
"enrich_example"

#' Example Multi-Database Enrichment Data
#'
#' @description
#' Example enrichment data from multiple databases.
#'
#' @format A data frame with enrichment results from multiple databases
#'
#' @name enrich_multidb_example
#' @docType data
#' @keywords datasets
"enrich_multidb_example"

#' Example GSEA Results
#'
#' @description
#' Example Gene Set Enrichment Analysis results.
#'
#' @format A data frame with GSEA results
#'
#' @name gsea_example
#' @docType data
#' @keywords datasets
"gsea_example"

#' Example Dimensionality Reduction Data
#'
#' @description
#' Example data for dimensionality reduction plots (e.g., UMAP, t-SNE).
#' Contains cell embeddings, velocity, and clustering information.
#'
#' @format A data frame with:
#' \describe{
#'   \item{Embeddings}{First 4 columns: x, y coordinates and velocity}
#'   \item{clusters}{Cell cluster assignments}
#'   \item{Additional metadata}{Additional grouping variables}
#' }
#'
#' @name dim_example
#' @docType data
#' @keywords datasets
"dim_example"

#' Excluded Words for Text Analysis
#'
#' @description
#' Common words to exclude from keyword enrichment and text mining.
#' Includes stop words and domain-specific common terms.
#'
#' @format A character vector of words to exclude
#'
#' @name words_excluded
#' @docType data
#' @keywords datasets
"words_excluded"
