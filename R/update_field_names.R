#' Convert UK Biobank field name to current format
#'
#' @description Internal function. Current format is [field]-[instance].[array]
#'   where instance is the visit and each array is an observation recorded under
#'   the field. The previous format was `@[field][instance].[array]`. For
#'   example, `@200021.14` becomes `20002-1.14` (the 15th observation recorded
#'   under field 20002 at the first follow-up visit).
#'
#' @param current Vector of field names.
#'
#' @return Vector of updated field names.
#'
#' @keywords internal
#' @noRd
#'
update_field_names <- function (current) {
  # Extract field, instance and array from previous format
  pattern <- "^@(\\d+)(\\d).(\\d+)$"
  components <- stringr::str_match(current, pattern)
  index_old <- which(!is.na(components[, 1]))
  # Update fields and return
  current[index_old] <- paste0(components[index_old, 2],
                               "-", components[index_old, 3],
                               ".", components[index_old, 4])
  current[]
}
