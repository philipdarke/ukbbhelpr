#' Extract field data from UK Biobank visit data
#'
#' @description Extracts all instances/arrays of data for a UK Biobank
#'   field(s) in clean "long" format (NOTE: watch for type coercion of
#'   different data types). See \href{https://biobank.ndph.ox.ac.uk/showcase/}{https://biobank.ndph.ox.ac.uk/showcase/}
#'   to identify field codes. Wrapper for \code{visit_fields()} which extracts
#'   raw field data.
#'
#' @param visit_data Data frame/table with UK Biobank data.
#' @param fields Vector of fields to extract e.g. \code{50} or
#'   \code{c(50, 21002)}. Field name will be identified from UK Biobank schema.
#'   Alternatively, field names can be set using a named vector e.g.
#'   \code{c("height" = 50, "weight" = 21002)}.
#' @param format Format of output table (\code{raw} or \code{source}). Default
#'    is currently raw but will change to source in a future release.
#'
#' @return Data table with values of all instances/arrays for each field in
#'   "long" format. The following columns are provided: \describe{
#'     \item{eid}{UK Biobank identifier.}
#'     \item{date}{Visit date.}
#'     \item{field/variable}{Field name (see below).}
#'     \item{array}{Provided if any fields have multiple arrays (more than one
#'     value recorded on the same date e.g repeated blood pressure).}
#'     \item{value}{Value recorded.}
#'   }
#'   If \code{format} = "source", an additional column \code{source} = "ukbb" is
#'   added to indicate data was recorded by UK Biobank and the field column is
#'   renamed \code{variable}. This will be the default in a future release. Use
#'   \code{format} = "raw" to keep current format.
#'
#' @examples
#' \dontrun{
#' # Load data
#' data_path <- ""  # add path to your data
#' visit_data <- fread(data_path)
#'
#' # Extract a field
#' visit_extract(visit_data, 50)
#'
#' # Extract multiple fields
#' visit_extract(visit_data, c(50, 21002))
#'
#' # Manually specify a field name
#' visit_extract(visit_data, c("height" = 50, 21002))
#' }
#'
#' @export
#'
visit_extract <- function (visit_data, fields, format = NULL) {
  eid = field = name = value = instance = n = NULL
  # Check arguments
  argument_check(visit_data, "data_table")
  argument_check(fields, "numeric", unique = TRUE)
  # Warn on output format
  if (is.null(format)) {
    format <- "raw"
    message('Output format will change in a future release. Use format = "raw" to keep current format.')
  } else {
    argument_check(format, "choice", choices = c("raw", "source"))
  }
  # Add missing field names from schema
  fields <- add_field_names(fields)
  field_names <- data.table::data.table(field = fields, name = names(fields))
  # Get instance dates
  all_dates <- visit_fields(visit_data, 53, format = "long")
  if (identical(unname(fields), 53)) {
    # Return dates only
    out <- merge(all_dates, field_names, by = "field")
    out <- out[, list(eid, field = name, value)]
  } else {
    # Return data with dates
    out <- visit_fields(visit_data, fields[fields != 53], format = "long")
    out <- merge(out, field_names, by = "field")
    out <- merge(out,
                 all_dates[, list(eid, field, instance, date = value)],
                 by = c("eid", "instance"))
    # Return array column if any field has multiple arrays
    out <- if (out[, list(n = length(array)), by = c("eid", "date", "name")][n > 1, length(n)] > 0) {
      if (format == "source") {
        out[, list(eid, date, variable = name, source = "ukbb", array, value)]
      } else {
        out[, list(eid, date, field = name, array, value)]
      }
    } else {
      if (format == "source") {
        out[, list(eid, date, variable = name, source = "ukbb", value)]
      } else {
        out[, list(eid, date, field = name, value)]
      }
    }
    out[, date := lubridate::as_date(date)]
  }
  out[]
}
