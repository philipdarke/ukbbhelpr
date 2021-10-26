#' Determine family history of a specified condition
#'
#' @description Determines presence of a specified condition in the self-reported
#'   family history data. If multiple history fields are provided (e.g. history
#'   of mother and father), presence of the condition in either field determines
#'   a positive family history.
#'
#' @param visit_data Data frame/table with UK Biobank data.
#' @param fields Vector of family history fields to extract e.g. one or more
#'   from \code{20107} (father), \code{20110} (mother) or \code{20111}
#'   (sibling).
#' @param condition Code for condition. Only a single condition at a time is
#'   currently supported. See \href{https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=1010}{https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=1010}
#'   to identify condition codes.
#' @param collapse Summarise results across all visit dates (default \code{TRUE}).
#'   If \code{FALSE}, the presence of the condition is provided at each visit
#'   date in the output table.
#' @param name Optional column name for condition (default \code{history}).
#'
#' @return Data table with \code{TRUE} (condition was reported), \code{FALSE}
#'   (condition was not reported) or \code{NA} (unknown/no response).
#'
#' @examples
#' \dontrun{
#' # Load data
#' data_path <- ""  # add path to your data
#' visit <- fread(data_path)
#'
#' # Extract history for father
#' visit_family_history(visit, 20107, 9)
#'
#' # Extract history for father, mother and siblings
#' visit_family_history(visit, c(20107, 20110, 20111), 9)
#'
#' # Name column in output
#' visit_family_history(visit, c(20107, 20110, 20111), 9, name = "diabetes")
#'
#' # Get history reported at each visit date
#' visit_family_history(visit, c(20107, 20110, 20111), 9, collapse = FALSE)
#' }
#'
#' @export
#'
visit_family_history <- function (visit_data, fields, condition, collapse = TRUE, name = NULL) {
  eid = value = NULL
  # Check fields input
  fields <- fields[fields %in% c(20107, 20110, 20111)]
  if (length(fields) == 0) {
    stop("Can only use family history fields (20107, 20110 and/or 20111).")
  }
  # Get data and check for condition at each date
  history_data <- visit_extract(visit_data, fields)
  out <-
  # Simplify across dates if collapse = TRUE
  out <- if (collapse) {
    history_data[,
                 list(history = query_family_history(value, condition)),
                 by = eid]
  } else {
    history_data[,
                 list(history = query_family_history(value, condition)),
                 by = c("eid", "date")]
  }
  # Update column names
  if (!is.null(name)) {
    out_names <- names(out)
    out_names[out_names == "history"] <- name
    names(out) <- out_names
  }
  out
}
