#' Extract self-reported cancer history
#'
#' @description Extracts self-reported cancer history in a "long" format that is
#'   easier to work with than "wide" as provided by UK Biobank.
#'
#' @param visit_data Data frame/table with UK Biobank data. Must include 20001 and
#'   20006 fields.
#'
#' @return Data table with \code{eid}, \code{date}, \code{condition} and
#'   \code{reported} columns. \code{date} is the reported date of diagnosis and
#'   \code{reported} is the date the cancer was self-reported to UK Biobank.
#'   \code{condition} is the cancer type coded as in
#'   \href{https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=3}{https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=3}.
#'
#' @export
#'
visit_cancer <- function (visit_data) {
  eid = condition = diagnosis = NULL
  # Extract conditions
  sr_data <- visit_extract(visit_data, c("condition" = 20001, "diagnosis" = 20006))
  # Drop unknown conditions and dates
  sr_data <- sr_data[condition != 99999]
  sr_data[diagnosis <= 0, diagnosis := NA]
  # Update and return
  sr_data[, list(eid,
                 date = lubridate::as_date(lubridate::date_decimal(diagnosis)),
                 condition,
                 reported = date)]
}
