#' Extract and clean raw observations/test results
#'
#' @description Internal function. Extracts values from \code{value1}/
#'   \code{value2} field. Units are taken from \code{value3} for data
#'   provider 2 (otherwise units are unavailable). \code{NA} and duplicate
#'   values are dropped. Zero values are dropped if \code{drop_zero} is TRUE
#'   (default).
#'
#' @param ehrs Data table/frame with UK Biobank clinical event data i.e.
#'   \code{gp_clinical.txt}.
#' @param drop_zero Drop codes with a zero numeric value (default TRUE).
#'
#' @return Data table with \code{value} and \code{unit} columns.
#'
#' @keywords internal
#' @noRd
#'
extract_pc_values <- function (data, drop_zero = TRUE) {
  data_provider = value = value1 = value2 = value3 = unit = NULL
  # Suppress NA warnings from as.numeric() calls
  warn_status <- getOption("warn")
  options(warn = -1)
  # Providers 1, 3 and 4
  data[, value := as.numeric(value1)]
  # Provider 2
  data[data_provider == 2 & is.na(value), value := as.numeric(value2)]
  # Return to initial warning status
  options(warn = warn_status)
  # Extract units
  data[data_provider == 2 & value3 != "", unit := value3]
  data[is.na(unit), unit := ""]
  # Drop invalid/missing values, value fields and duplicates
  data <- data[!is.na(value)]
  if (drop_zero) { data <- data[value != 0] }
  data[, paste0("value", 1:3) := NULL]
  data <- unique(data)
  data[]
}
