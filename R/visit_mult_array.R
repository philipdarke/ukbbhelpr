#' Extract data from two UK Biobank fields jointly
#'
#' @description Some fields relate to each other e.g. self-reported medical
#' history where field 20002 contains the disclosed conditions and 20008 the
#' date of diagnosis. The date in array \code{i} of 20008 corresponds to the
#' condition in array \code{i} of 20002. \code{visit_mult_array()} jointly
#' extracts such fields in a "long" format that is easier to work with than
#' "wide" as provided by UK Biobank (NOTE: watch for type coercion of
#' different data types).
#'
#' @param visit_data Data frame/table with UK Biobank data.
#' @param fields Vector of fields to extract e.g. \code{c(50, 21002)}. Must
#'   be length two. Field name will be identified from UK Biobank schema.
#'   Alternatively, field names can be set using a named vector e.g.
#'   \code{c("height" = 50, "weight" = 21002)}.
#'
#' @return Data table with \code{eid}, \code{reported}, and columns
#'   corresponding to the \code{fields} argument. \code{reported} is the date
#'   corresponding to the field instance e.g. the UK Biobank visit at which the
#'   data was collected. Each row shows the data for an array.
#'
#' @export
#'
visit_mult_array <- function (visit_data, fields) {
  eid = field = value = NULL
  # Check input
  if (length(fields) != 2) {
    stop('Argument "fields" must be length 2.')
  }
  # Update field names
  fields <- add_field_names(fields)
  fields_new <- unname(fields)
  fields_new <- c("a" = fields_new[1], "b" = fields_new[2])
  # Extract and combine field data
  data_raw <- visit_extract(visit_data, fields_new)
  table_a <- data_raw[field == "a",
                      list(eid, reported = date, array, a = value)]
  table_b <- data_raw[field == "b",
                      list(eid, reported = date, array, b = value)]
  out <- merge(table_a, table_b, by = c("eid", "reported", "array"), all = TRUE)
  # Rename columns
  setnames(out, names(fields_new), names(fields))
  # Drop array column and return
  out[, array := NULL]
  out[]
}
