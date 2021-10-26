#' Extract observations/test results from raw EHR data
#'
#' @description Extracts values from linked EHR data. Data is extracted from
#'   the \code{value1} field with the exception of data provider 2 where values
#'   are extracted from \code{value2} if \code{value1} is empty. Units are taken
#'   from \code{value3} for data provider 2 (otherwise units are unavailable).
#'   \code{NA}, zero and duplicate values are dropped.
#'
#' @param ehr_data Data table/frame with UK Biobank clinical event data i.e.
#'   \code{gp_clinical.txt}.
#' @param read_codes Values are extracted from these codes. Data table/frame
#'   with Read v2 (\code{read_2}) and CTV3 (\code{read_3}) columns.
#'
#' @return Data table with \code{value} and \code{unit} columns.
#'
#' @export
#'
ehr_extract <- function (ehr_data, read_codes) {
  read_2 = read_3 = NULL
  # Check inputs
  ehr_data <- check_gp_clinical(ehr_data)
  codes <- check_read_codes(read_codes)
  # Extract and clean observations/test results
  raw_values <- ehr_data[read_2 %in% setdiff(read_codes$read_2, "-") |
                           read_3 %in% setdiff(read_codes$read_3, "-")]
  extract_pc_values(raw_values)
}
