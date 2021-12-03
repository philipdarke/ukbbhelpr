#' Extract EHR records and values
#'
#' @description Extracts records/values from linked EHR data. If \code{values}
#'   is TRUE, values are extracted the \code{value1} field with the exception of
#'   data provider 2 where values are extracted from \code{value2} if
#'   \code{value1} is empty. Units are taken from \code{value3} for data
#'   provider 2 (otherwise units are unavailable). \code{NA}, zero and duplicate
#'   values are dropped.
#'
#' @param ehr_data Data table with UK Biobank clinical event data i.e.
#'   \code{gp_clinical.txt}.
#' @param read_codes Extract records matching these codes. Data table/frame
#'   with \code{read_2} (Read v2) and \code{read_3} (CTV3) columns.
#' @param format Set to "values" to extract values from EHR records. Replaces the
#'   \code{value1}, \code{value2}, \code{value3} columns with a single \code{value}
#'   column and a \code{unit} column. Default is currently "values" but will change
#'   in a future release.
#'
#' @return Filtered EHR data with \code{value} and \code{unit} columns if
#'   \code{format} is "values".
#'
#' @export
#'
ehr_extract <- function (ehr_data, read_codes, format = NULL) {
  read_2 = read_3 = NULL
  # Check arguments
  argument_check(ehr_data, "data_table")
  ehr_data <- check_gp_clinical(ehr_data)
  codes <- check_read_codes(read_codes)
  # Warn on format argument
  if (is.null(format)) {
    format <- "values"
    message('Argument "format" will change default in a future release. Use format = "values" to keep current behaviour.')
  } else {
    argument_check(format, "choice", choices = c("raw", "values"))
  }
  # Extract and clean observations/test results
  raw_values <- ehr_data[read_2 %in% setdiff(read_codes$read_2, "-") |
                           read_3 %in% setdiff(read_codes$read_3, "-")]
  if (format == "values") {
    extract_pc_values(raw_values)
  } else {
    raw_values[]
  }
}
