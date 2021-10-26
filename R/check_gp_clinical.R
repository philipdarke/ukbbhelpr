#' Check primary care clinical event data
#'
#' @description Internal function. Checks all UK Biobank columns are present in
#'   primary care clinical event data and coerces input to data table.
#'
#' @param data Data frame/table to check.
#'
#' @return Data table.
#'
#' @keywords internal
#' @noRd
#'
check_gp_clinical <- function(data) {
  # Check all UK Biobank variables are provided
  ukbb_variables <- c("eid", "data_provider", "event_dt", "read_2", "read_3",
                      "value1", "value2", "value3")
  if (sum(names(data) %in% ukbb_variables) != length(ukbb_variables)) {
    stop("Input data does not contain all columns in gp_clinical.txt.")
  }
  # Coerce input to data table if needed
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  data[]
}
