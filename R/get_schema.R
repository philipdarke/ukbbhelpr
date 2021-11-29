#' Download and return UK Biobank schema
#'
#' @description Internal function. Schema URL is \href{https://biobank.ctsu.ox.ac.uk/ukb/scdown.cgi?fmt=txt&id=16}{https://biobank.ctsu.ox.ac.uk/ukb/scdown.cgi?fmt=txt&id=16}.
#'
#' @param overwrite Overwrite existing file (default FALSE).
#'
#' @keywords internal
#' @noRd
#'
get_schema <- function (overwrite = FALSE) {
  url = "https://biobank.ctsu.ox.ac.uk/ukb/scdown.cgi?fmt=txt&id=16"
  path = "schema"
  # Get schema
  if (!download_file(url, path, overwrite)) {
    stop("Could not download file.")
  }
  # Check file
  file_head <- readLines(path, n = 1)
  if (substr(file_head[1], 1, 8) != "field_id") {
    # Delete file if corrupted
    message("File is corrupt.")
    file.remove(path)
  } else {
    # Save schema as data table
    schema <- data.table::fread(path)
    data.table::fwrite(schema, path)
    schema[]
  }
}
