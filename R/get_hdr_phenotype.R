#' Get phenotype from HDR UK phenotype library
#'
#' Queries HDR UK phenotype library for a given phenotype. Read v2 codes are
#' mapped to CTV3 if the \code{read2} argument is passed.
#'
#' @param id Phenotype ID.
#' @param version_id Phenotype version ID.
#' @param api Optional HttpClient to connect to phenotype library (default uses
#'   \href{https://phenotypes.healthdatagateway.org/}{https://phenotypes.healthdatagateway.org/}).
#' @param read2 Optional vector of terms corresponding to Read v2 codes in the
#'   \code{coding_system} field. If provided, rows with these terms are mapped
#'   to CTV3 using UK Biobank mapping dictionaries.
#'
#' @return Data table with phenotype codes.
#'
#' @export
#'
get_hdr_phenotype <- function (id, version_id, api = NULL, read2 = NULL) {
  read_2 = read_3 = code = coding_system = description = NULL
  code_attributes.Disease = code_attributes.Category =  phenotype_id = NULL
  phenotype_version_id = phenotype_name = NULL
  # Check arguments
  argument_check(id, "string")
  argument_check(version_id, "number")
  argument_check(read2, "character", null.ok = TRUE)
  # Get HDR phenotype
  if (is.null(api)) {
    api <- ConceptLibraryClient::connect_to_API(url = HDR_API)
  }
  hdr_codes <- ConceptLibraryClient::get_phenotype_code_list(id, version_id, api_client = api)
  hdr_codes <- data.table::as.data.table(hdr_codes)
  # Add Read v2 codes and map to CTV3
  if (!is.null(read2)) {
    hdr_codes[coding_system %in% read2, read_2 := substr(code, 1, 5)]
    ctv3_map <- ehr_map_codes(hdr_codes$read_2)
    hdr_codes <- merge(hdr_codes,
                       ctv3_map[, list(read_2, read_3)],
                       by = "read_2", all.x = TRUE)

  }
  hdr_codes <- hdr_codes[, list(read_2, read_3, code, coding_system,
                                description, code_attributes.Disease, code_attributes.Category,
                                phenotype_id, phenotype_version_id, phenotype_name)]
  hdr_codes <- hdr_codes[order(coding_system, code)]
  hdr_codes[]
}
