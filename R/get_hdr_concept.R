#' Get concept from HDR UK phenotype library
#'
#' Queries HDR UK phenotype library for a given concept. Read v2 codes are
#' mapped to CTV3 if the \code{read} argument is TRUE.
#'
#' @param id Concept ID.
#' @param version_id Optional concept version ID. Otherwise latest version of
#'   concept is returned.
#' @param api Optional HttpClient to connect to phenotype library (default uses
#'   \href{https://phenotypes.healthdatagateway.org/}{https://phenotypes.healthdatagateway.org/}).
#' @param read Set to TRUE if concept contains Read v2 codes. Read v2 codes of
#'   length less than 5 are padded with a "." e.g. "C10" becomes "C10..". Codes
#'   are mapped to CTV3 using UK Biobank mapping dictionaries.
#'
#' @return Data table with phenotype codes. \code{read_2} (Read v2) and
#'   \code{read_3} (CTV3) columns are added if \code{read} is TRUE.
#'
#' @export
#'
get_hdr_concept <- function (id, version_id = NULL, api = NULL, read = FALSE) {
  read_2 = read_3 = code = NULL
  # Check arguments
  argument_check(id, "string")
  argument_check(read, "flag")
  if (is.null(api)) {
    api <- ConceptLibraryClient::connect_to_API(url = HDR_API)
  }
  if (is.null(version_id)) {
    versions <- ConceptLibraryClient::get_concept_versions(id = id, api_client = api)
    version_id <- versions[which(versions$is_latest), "version_id"]
  } else {
    argument_check(version_id, "number")
  }
  # Get HDR concept
  hdr_codes <- ConceptLibraryClient::get_concept_code_list_by_version(id = id,
                                                                      version_id = version_id,
                                                                      api_client = api)
  hdr_codes <- data.table::as.data.table(hdr_codes)
  # Add Read v2 codes and map to CTV3
  if (read) {
    hdr_codes[, read_2 := substr(code, 1, 5)]
    hdr_codes[, read_2 := stringr::str_pad(read_2, width = 5, side = "right", pad = ".")]
    ctv3_map <- ehr_map_codes(hdr_codes$read_2)
    hdr_codes <- merge(hdr_codes,
                       ctv3_map[, list(read_2, read_3)],
                       by = "read_2", all.x = TRUE)
    n_cols <- ncol(hdr_codes)
    data.table::setcolorder(hdr_codes, c(2:(n_cols - 1), 1, n_cols))

  }
  hdr_codes[]
}
