#' Download file
#'
#' @description Internal function.
#'
#' @param url URL for file.
#' @param path Path to save file.
#' @param overwrite Overwrite existing file (default FALSE).
#'
#' @return TRUE if download successful, else FALSE.
#'
#' @keywords internal
#' @noRd
#'
download_file <- function(url, path, overwrite = FALSE) {
  # Handle download errors
  error_handler <- function () {
    file.remove(path)
    return(FALSE)
  }
  if (file.exists(path) & overwrite == FALSE) {
    # Exit if already downloaded and not overwriting file
    return(TRUE)
  } else {
    # Download file
    tryCatch({
      test = utils::download.file(url, path)
      return(TRUE)
    },
    warning = function (w) {
      error_handler()
    },
    error = function (e) {
      error_handler()
    })
  }
}
