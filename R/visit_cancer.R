#' Extract self-reported cancer history
#'
#' @description Extracts self-reported cancer history in a "long" format that is
#'   easier to work with than "wide" as provided by UK Biobank (NOTE: watch for
#'   type coercion of different data types). Wrapper function for
#'   \code{visit_mult_array()}.
#'
#' @param visit_data Data frame/table with UK Biobank data. Must include fields
#'   \code{20001} and \code{20006}.
#'
#' @return Data table with the following columns: \describe{
#'   \item{eid}{UK Biobank identifier.}
#'   \item{date}{Reported date of diagnosis.}
#'   \item{condition}{Cancer type coded as in
#'   \href{https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=3}{https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=3}.}
#'   \item{desc}{Description of cancer type (added if coding data can be downloaded).}
#'   \item{reported}{Visit date at which the cancer was self-reported.}
#' }
#'
#' @export
#'
visit_cancer <- function (visit_data) {
  eid = condition = reported = coding = desc = meaning = NULL
  # Check arguments
  argument_check(visit_data, "data_table")
  # Extract conditions
  sr_data <- visit_mult_array(visit_data, c("condition" = 20001, "date" = 20006))
  # Drop unknown conditions and dates
  sr_data <- sr_data[condition != 99999]
  sr_data[date <= 0, date := NA]
  # Get condition names and return
  out <- sr_data[, list(eid,
                        date = lubridate::as_date(lubridate::date_decimal(date)),
                        condition,
                        reported)]
  tryCatch({
    coding6 <- get_coding(3)
    out <- merge(out,
                 coding6[coding > 0, list(condition = coding, desc = meaning)],
                 by = "condition", all.x = TRUE)
    out <- out[, list(eid, date, condition, desc, reported)]
  },
  error = function(e) {
    message("Could not add cancer description field.")
  })
  out[]
}
