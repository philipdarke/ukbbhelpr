#' Split UK Biobank field code
#'
#' @description Internal function. Split field code into field, instance and array.
#'
#' @param code Vector of codes to split e.g. c("53-0.0", "20002.1.14")
#'
#' @return Data table with a row for each input and \code{field},
#'   \code{instance} and \code{array} columns.
#'
#' @keywords internal
#' @noRd
#'
split_field_code <- function (code) {
  codes <- stringr::str_match(code, "^(\\d+)-(\\d+).(\\d+)$")
  codes <- matrix(as.numeric(codes[, -1]), ncol = 3)
  codes <- data.table::data.table(codes)
  names(codes) <- c("field", "instance", "array")
  codes[]
}
