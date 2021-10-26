#' Extract raw fields from visit data
#'
#' @description Internal function. Extracts all instances/arrays of data from a
#'   UK Biobank field. See \code{https://biobank.ndph.ox.ac.uk/showcase/} to
#'   identify field codes.
#'
#' @param data Data frame/table with UK Biobank data.
#' @param fields Vector of fields to extract e.g. \code{50} or
#'   \code{c(50, 21002)}.
#'
#' @return Data table with values of all instances/arrays for each field.
#'
#' @keywords internal
#' @noRd
#'
visit_fields <- function (data, fields) {
  # Check all fields have at least one column in data
  num_columns <- sapply(fields, function (field) {
    length(stringr::str_which(names(data), paste0("^", field, "-")))
  })
  if (any(num_columns < 1)) {
    stop("Check field codes are correct. Are all fields in the data?")
  }
  # Extract fields
  names(data) <- update_field_names(names(data))
  pattern <- paste0("^", fields, "-\\d\\.\\d+$", collapse = "|")
  pattern <- paste0("^eid$|", pattern)
  data[, stringr::str_which(names(data), pattern), with = FALSE]
}
