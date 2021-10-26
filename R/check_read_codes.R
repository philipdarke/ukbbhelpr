#' Check Read v2/CTV3 code sets
#'
#' @description Internal function. Checks that Read v2 and CTV3 codes of length
#'   5 characters are present in the code table and coerces input to data table.
#'
#' @param codes Data frame/table to check.
#'
#' @return Data table.
#'
#' @keywords internal
#' @noRd
#'
check_read_codes <- function(codes) {
  read_2 = read_3 = NULL
  # Check read_2 and read_3 codes provided
  if (sum(names(codes) %in% c("read_2", "read_3")) != 2) {
    stop("Input data does not read_2 and read_3 columns.")
  }
  # Coerce input to data table if needed
  if (!data.table::is.data.table(codes)) {
    codes <- data.table::as.data.table(codes)
  }
  # Check code lengths
  if (any(codes[read_2 != "-", stringr::str_length(read_2)] != 5) |
      any(codes[read_3 != "-", stringr::str_length(read_3)] != 5)) {
    stop("Read v2 / CTV3 codes must be of length 5.")
  }
  codes[]
}
