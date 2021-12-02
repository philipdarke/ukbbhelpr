#' Subset fields from UK Biobank visit data
#'
#' @description Loads UK Biobank data and subsets required fields. Use to avoid
#'   loading full data each time. See \href{https://biobank.ndph.ox.ac.uk/showcase/}{https://biobank.ndph.ox.ac.uk/showcase/}
#'   to identify field codes.
#'
#' @param data_path Path to raw UK Biobank data unpacked using \code{ukbunpack}
#'   utility.
#' @param fields Vector of fields to extract e.g. \code{50} or
#'   \code{c(50, 21002)}.
#' @param ... Passed to \code{fread} e.g. to set file separator.
#' @param save Optional path to save output.
#'
#' @return Data table with all instances/arrays for each field. Note the date
#'   field (53) is always returned.
#'
#' @export
#'
visit_subset <- function(data_path, fields, ..., save = NULL) {
  # Check arguments
  argument_check(data_path, "string")
  argument_check(fields, "numeric", unique = TRUE)
  argument_check(save, "string", null.ok = TRUE)
  # Load data
  data_cols <- data.table::fread(data_path, nrows = 1, header = TRUE, ...)
  # Determine column numbers
  names(data_cols) <- update_field_names(names(data_cols))
  pattern <- paste0("^", c(53, fields), "-\\d\\.\\d+$", collapse = "|")  # 53 = visit date
  pattern <- paste0("^eid$|", pattern)
  fields_index <- stringr::str_which(names(data_cols), pattern)
  # Return subset
  out <- data.table::fread(data_path, select = fields_index, header = TRUE)
  if (!is.null(save)) {
    fwrite(out, save)
    message(paste("File saved to", save))
  } else {
    out[]
  }
}
