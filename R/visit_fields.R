#' Extract raw field data from UK Biobank visit data
#'
#' @description Extracts all instances/arrays of data for a UK Biobank
#'   field(s). See \href{https://biobank.ndph.ox.ac.uk/showcase/}{https://biobank.ndph.ox.ac.uk/showcase/}
#'   to identify field codes.
#'
#' @param visit_data Data frame/table with UK Biobank data.
#' @param fields Vector of fields to extract e.g. \code{50} or
#'   \code{c(50, 21002)}.
#'
#' @return Data table with all instances/arrays for each field in UK Biobank
#'   "wide" format.
#'
#' @examples
#' \dontrun{
#' # Load data
#' data_path <- ""  # add path to your data
#' visit_data <- fread(data_path)
#'
#' # Extract a field
#' visit_fields(visit_data, 50)
#'
#' # Extract multiple fields
#' visit_fields(visit_data, c(50, 21002))
#' }
#'
#' @export
#'
visit_fields <- function (visit_data, fields) {
  # Check all fields have at least one column in data
  num_columns <- sapply(fields, function (field) {
    length(stringr::str_which(names(visit_data), paste0("^", field, "-")))
  })
  if (any(num_columns < 1)) {
    stop("Check field codes are correct. Are all fields in the data?")
  }
  # Extract fields
  names(visit_data) <- update_field_names(names(visit_data))
  pattern <- paste0("^", fields, "-\\d\\.\\d+$", collapse = "|")
  pattern <- paste0("^eid$|", pattern)
  visit_data[, stringr::str_which(names(visit_data), pattern), with = FALSE]
}
