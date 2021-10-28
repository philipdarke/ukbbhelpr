#' Download UK Biobank schema
#'
#' @description Internal function. Schema URL is \href{http://biobank.ctsu.ox.ac.uk/ukb/scdown.cgi?fmt=txt&id=16}{http://biobank.ctsu.ox.ac.uk/ukb/scdown.cgi?fmt=txt&id=16}.
#'
#' @keywords internal
#' @noRd
#'
get_schema <- function() {
  path <- "schema"
  if (file.exists(path)) {
    schema <- data.table::fread(path)
  } else {
    url <- "http://biobank.ctsu.ox.ac.uk/ukb/scdown.cgi?fmt=txt&id=16"
    error_msg <- "Could not download UK Biobank schema."
    tryCatch({
      message("Downloading UK Biobank schema...")
      utils::download.file(url, path)
    },
    warning = function (w) {
      message(error_msg)
      stop(w)
    },
    error = function (e) {
      message(error_msg)
      stop(e)
    })
    # Check file
    file_contents <- readLines(path, n = 1)
    if (substr(file_contents[1], 1, 8) != "field_id") {
      message("Failed to parse schema.")
      file.remove(path)
    } else {
      # Save schema as data table
      schema <- data.table::fread(path)
      schema_names <- names(schema)
      schema_names[1:2] <- c("field", "name")
      names(schema) <- schema_names
      data.table::fwrite(schema, path)
    }
  }
  schema
}
