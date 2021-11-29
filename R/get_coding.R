#' Get UK Biobank coding
#'
#' Returns the UK Biobank coding file for the supplied \code{id}. The file is downloaded if it is unavailable locally.
#'
#' @param id ID for coding file. Obtain this from the data dictionary for your application, or directly from the Data Showcase (\href{https://biobank.ndph.ox.ac.uk/showcase/}{https://biobank.ndph.ox.ac.uk/showcase/}), by looking up the relevant data field.
#' @param overwrite Overwrite existing file (default FALSE).
#'
#' @return Data table with contents of coding file.
#'
#' @export
#'
get_coding <- function (id, overwrite = FALSE) {
  url = paste0("https://biobank.ctsu.ox.ac.uk/ukb/codown.cgi?id=", id)
  path = paste0("coding", id)
  # Get schema
  if (!download_file(url, path, overwrite)) {
    stop("Could not download file.")
  }
  # Check file
  file_head <- readLines(path, n = 1)
  if (substr(file_head[1], 1, 6) != "coding") {
    # Delete file if corrupted
    message("File is corrupt.")
    file.remove(path)
  } else {
    # Save schema as data table
    coding <- data.table::fread(path)
    data.table::fwrite(coding, path)
    coding[]
  }
}
