#' Extract field data from UK Biobank visit data
#'
#' @description Extracts all instances/arrays of data for a UK Biobank
#'   field(s) in clean "long" format. See \href{https://biobank.ndph.ox.ac.uk/showcase/}{https://biobank.ndph.ox.ac.uk/showcase/}
#'   to identify field codes.
#'
#' @param visit_data Data frame/table with UK Biobank data.
#' @param fields Vector of fields to extract e.g. \code{50} or
#'   \code{c(50, 21002)}. Field name will be identified from UK Biobank schema.
#'   Alternatively, field names can be set using a named vector e.g.
#'   \code{c("height" = 50, "weight" = 21002)}.
#'
#' @return Data table with values of all instances/arrays for each field in
#'   "long" format i.e. \code{eid}, \code{date} of visit, \code{field} name and
#'   \code{value} recorded by UK Biobank.
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
visit_extract <- function (visit_data, fields) {
  eid = field = name = NULL
  # Drop date from fields
  fields <- fields[fields != 53]
  # Add missing field names from schema
  field_names <- names(fields)
  if (is.null(field_names) | any(field_names == "")) {
    schema <- get_schema()
    if (is.null(field_names)) {
      field_names <- schema[field %in% fields, name]
    } else {
      missing_names <- which(field_names == "")
      field_names[missing_names] <- schema[field %in% fields[missing_names], name]
    }
    names(fields) <- field_names
  }
  # Coerce input to data table if needed
  if (!data.table::is.data.table(visit_data)) {
    visit_data <- data.table::as.data.table(visit_data)
  }
  # Get all data
  all_data <- visit_fields(visit_data, c(53, fields))  # 53 = visit date
  all_data_names <- names(all_data)
  # For each field...
  fields_data <- lapply(fields, function (field) {
    # ...identify instances
    field_name <- names(fields[fields == field])
    instances <- stringr::str_match(all_data_names, paste0("^", field, "-(\\d+).(\\d+)$"))
    instances <- unique(instances[, 2])
    instances <- instances[!is.na(instances)]
    # For each instance...
    instances_data <- lapply(instances, function (instance) {
      # Get data
      date_code <- paste0("53-", instance, ".0")
      pattern <- paste0("^", c(53, field), "-", instance, "\\.\\d+$", collapse = "|")
      pattern <- paste0("^eid$|", pattern)
      instance_data <- all_data[, stringr::str_which(all_data_names, pattern), with = FALSE]
      # Form long data table
      suppressWarnings(
        # Suppress type coercion warnings
        instance_data <- data.table::melt(instance_data,
                                          id.vars = c("eid", date_code),
                                          variable.name = "field",
                                          variable.factor = FALSE,
                                          na.rm = TRUE)
      )
      instance_data[, field := field_name]
      # Update column names
      instance_names <- names(instance_data)
      instance_names[instance_names == date_code] <- "date"
      names(instance_data) <- instance_names
      # Return table
      instance_data
    })
    # Combine data tables for each instance
    data.table::rbindlist(instances_data)
  })
  # Combine data tables for each field
  out <- data.table::rbindlist(fields_data)
  # Format dates
  out[, date := lubridate::as_date(date)]
  # Return data
  out <- out[order(eid, field, date)]
  out[]
}
