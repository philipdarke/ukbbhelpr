#' Add field names to vector of field codes
#'
#' @description Internal function.
#'
#' @param fields Vector of fields to extract e.g. \code{50} or
#'   \code{c(50, 21002)}. Vector can be partially named e.g.
#'   \code{c("height" = 50, 21002)} in which case only unnamed fields will be
#'   updated.
#'
#' @keywords internal
#' @noRd
#'
add_field_names <- function (fields) {
  field_id = title = NULL
  field_names <- names(fields)
  if (is.null(field_names) | any(field_names == "")) {
    schema <- get_schema()
    if (is.null(field_names)) {
      field_names <- schema[field_id %in% fields, title]
    } else {
      missing_names <- which(field_names == "")
      field_names[missing_names] <- schema[field_id %in% fields[missing_names], title]
    }
    names(fields) <- field_names
  }
  fields
}
