#' Identify reported family history of a condition
#'
#' @description  Internal function. Note conditions fall in two groups
#' e.g. Y19 (group 1) and Y20 (group 2). See \href{https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=113241}{https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=113241}
#' for more details.
#'
#' @param conditions Condition codes for participant.
#' @param query Code for condition. See \href{https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=1010}{https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=1010}
#'   to identify conditions codes.
#'
#' @return TRUE/FALSE/NA for presence of condition.
#'
#' @keywords internal
#' @noRd
#'
query_family_history <- function (conditions, query) {
  if (length(query) > 1) {
    stop("Can only query a single condition.")
  }
  # Unknown/prefer not to answer codes for group containing query
  unknown_codes <- if (query %in% c(1, 2, 6, 8, 9, 10)) {
    c(-11, -13)  # unknown/decline (group 1)
  } else if (query %in% c(3, 4, 5, 11, 12, 13)) {
    c(-21, -23)  # unknown/decline (group 2)
  } else {
    stop("Invalid condition code.")
  }
  # Test for condition
  if (any(conditions == query)) {
    return(TRUE)
  } else if (any(conditions %in% unknown_codes)) {
    return(NA)
  } else {
    return(FALSE)
  }
}

