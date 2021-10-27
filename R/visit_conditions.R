#' Extract self-reported non-cancer medical history
#'
#' @description Extracts self-reported non-cancer medical history in a "long"
#'   format that is easier to work with than "wide" as provided by UK Biobank.
#'
#' @param visit_data Data frame/table with UK Biobank data. Must include fields
#'   \code{20002} and \code{20008}.
#'
#' @return Data table with \code{eid}, \code{date}, \code{condition} and
#'   \code{reported} columns. \code{date} is the reported date of diagnosis and
#'   \code{reported} is the date the condition was self-reported to UK Biobank.
#'   \code{condition} is the health condition coded as in
#'   \href{https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=6}{https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=6}.
#'
#' @export
#'
visit_conditions <- function (visit_data) {
  eid = condition = field = value = reported = NULL
  # Extract conditions
  sr_data <- visit_extract(visit_data, c("condition" = 20002, "date" = 20008))
  # Combine condition and date of diagnosis
  condition <- sr_data[field == "condition",
                       list(eid, reported = date, array, condition = value)]
  diagnosis <- sr_data[field == "date",
                       list(eid, reported = date, array, date = value)]
  sr_data <- merge(condition, diagnosis, by = c("eid", "reported", "array"), all = TRUE)
  # Drop unknown conditions and dates
  sr_data <- sr_data[condition != 99999]
  sr_data[date <= 0, date := NA]
  # Update and return
  sr_data[, list(eid,
                 date = lubridate::as_date(lubridate::date_decimal(date)),
                 condition,
                 reported)]
}
