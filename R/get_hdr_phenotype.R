#' Get phenotype from HDR UK phenotype library
#'
#' Queries HDR UK phenotype library for a given phenotype.
#'
#' @param id Phenotype ID.
#' @param version_id Optional phenotype version ID. Otherwise latest version of
#'   phenotype is returned.
#' @param api Optional HttpClient to connect to phenotype library (default uses
#'   \href{https://phenotypes.healthdatagateway.org/}{https://phenotypes.healthdatagateway.org/}).
#'
#' @return Data table with phenotype codes.
#'
#' @export
#'
get_hdr_phenotype <- function (id, version_id = NULL, api = NULL) {
  # Check arguments
  argument_check(id, "string")
  if (is.null(api)) {
    api <- ConceptLibraryClient::connect_to_API(url = HDR_API)
  }
  if (is.null(version_id)) {
    versions <- ConceptLibraryClient::get_phenotype_versions(id = id, api_client = api)
    version_id <- versions[which(versions$is_latest), "version_id"]
  } else {
    argument_check(version_id, "number")
  }
  # Get HDR phenotype
  hdr_codes <- ConceptLibraryClient::get_phenotype_code_list(id = id,
                                                             version_id = version_id,
                                                             api_client = api)
  hdr_codes <- data.table::as.data.table(hdr_codes)
  hdr_codes[]
}
