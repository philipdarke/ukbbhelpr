#' Extract EHR records and values using a HDR UK phenotype library concept
#'
#' @description Extracts records/values from linked EHR data. If \code{values}
#'   is TRUE, values are extracted the \code{value1} field with the exception of
#'   data provider 2 where values are extracted from \code{value2} if
#'   \code{value1} is empty. Units are taken from \code{value3} for data
#'   provider 2 (otherwise units are unavailable). \code{NA}, zero and duplicate
#'   values are dropped. Wrapper for \code{ehr_extract()}.
#'
#' @param ehr_data Data table with UK Biobank clinical event data i.e.
#'   \code{gp_clinical.txt}.
#' @param id Concept ID.
#' @param version_id Optional concept version ID. Otherwise latest version of
#'   concept is returned.
#' @param api Optional HttpClient to connect to phenotype library (default uses
#'   \href{https://phenotypes.healthdatagateway.org/}{https://phenotypes.healthdatagateway.org/}).
#' @param format Set to "values" to extract values from EHR records. Replaces the
#'   \code{value1}, \code{value2}, \code{value3} columns with a single \code{value}
#'   column and a \code{unit} column. Default is currently "values" but will change
#'   in a future release.
#'
#' @return Filtered EHR data with \code{value} and \code{unit} columns if
#'   \code{format} is "values".
#'
#' @export
#'
ehr_hdr_extract <- function (ehr_data, id, version_id = NULL, api = NULL, format = "raw") {
  hdr_codes <- get_hdr_concept(id = id, version_id = version_id, api = api, read = TRUE)
  out <- ehr_extract(ehr_data = ehr_data, read_codes = hdr_codes, format = format)
  out[]
}

