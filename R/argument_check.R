#' Check function argument
#'
#' @description Internal function.
#'
#' @param argument Argument to check.
#' @param check Type of argument to test. Currently supports "data_table",
#'   "flag", "scalar", "numeric", "string" or "character".
#' @param ... Passed to \code{checkpoint} test/check functions.
#'
#' @return Silent unless argument check fails.
#'
#' @keywords internal
#' @noRd
#'
argument_check <- function (argument, check, ...) {
  # Set up argument check
  checks <- c("data_table", "flag", "scalar", "number", "numeric", "string",
              "character", "choice", "subset")
  if (!checkmate::test_choice(check, checks)) {
    stop(paste0("Argument check ", tolower(checkmate::check_choice(check, checks)), "."))
  }
  tester <- switch(check,
                   "data_table" = checkmate::test_data_table,
                   "flag" = checkmate::test_flag,
                   "scalar" = checkmate::test_scalar,
                   "number" = checkmate::test_number,
                   "numeric" = checkmate::test_numeric,
                   "string" = checkmate::test_string,
                   "character" = checkmate::test_character,
                   "choice" = checkmate::test_choice,
                   "subset" = checkmate::test_subset)
  checker <- switch(check,
                    "data_table" = checkmate::check_data_table,
                    "flag" = checkmate::check_flag,
                    "scalar" = checkmate::check_scalar,
                    "number" = checkmate::check_number,
                    "numeric" = checkmate::check_numeric,
                    "string" = checkmate::check_string,
                    "character" = checkmate::check_character,
                    "choice" = checkmate::check_choice,
                    "subset" = checkmate::check_subset)
  # Check argument
  argument_name <- checkmate::vname(argument)
  if (!tester(argument, ...)) {
    stop(paste0("Argument ", argument_name, " ", tolower(checker(argument, ...)), "."))
  }
}
