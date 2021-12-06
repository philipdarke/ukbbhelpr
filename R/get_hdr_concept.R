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
#' @return Data table with phenotype codes. \code{read_2}/\code{term_2}
#'   and \code{read_3}/\code{term_3} columns are added with codes and terms
#'   under Read v2 and CTV3 if \code{read} is TRUE.
#'
#' @export
#'
get_hdr_concept <- function (id, version_id = NULL, api = NULL, read = FALSE) {
  read_2 = read_3 = code = desc = NULL
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
    ctv3_map <- ehr_map_codes(hdr_codes$code)
    hdr_codes <- merge(hdr_codes, ctv3_map, by = "code", all.x = TRUE)
    hdr_codes[, desc := NULL]
  }
  hdr_codes[]
}
