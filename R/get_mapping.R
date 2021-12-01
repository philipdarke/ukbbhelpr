#' Download UK Biobank primary care mapping dictionaries
#'
#' @description Internal function. Schema URL is \href{https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=592}{https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=592}.
#'
#' @param overwrite Overwrite existing file (default FALSE).
#'
#' @keywords internal
#' @noRd
#'
get_mapping <- function (overwrite = FALSE) {
  url <- "https://biobank.ndph.ox.ac.uk/showcase/ukb/auxdata/primarycare_codings.zip"
  path <- "map_all"
  # Get schema
  if (!download_file(url, path, overwrite)) {
    stop("Could not download file.")
  }
}
