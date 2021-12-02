#' Map EHR codes to another terminology
#'
#' Map EHR codes from one terminology to another using UK Biobank mapping
#' dictionaries. Currently supports mapping Read v2 to CTV 3 only.
#'
#' @param codes Vector of codes to map.
#' @param from Terminology to map from. Currently supports Read v2 codes only
#'   (first 5 digits excluding the final 2 digit term). Full 5 digit codes
#'   must be passed i.e. no partial matching.
#' @param to Terminology to map to. Currently supports CTV3 only.
#' @param overwrite Overwrite existing mapping dictionary (default FALSE).
#'
#' @return Data table with mapped codes and description.
#'
#' @export
#'
ehr_map_codes <- function (codes, from = "read2", to = "ctv3", overwrite = FALSE) {
  READV2_CODE = READV3_CODE = TERMV3_DESC = TERMV3_TYPE = read_2 = read_3 = desc = NULL
  # Check arguments
  argument_check(codes, "character")
  argument_check(overwrite, "flag")
  if (from != "read2" | to != "ctv3") {
    stop("Only mapping from Read v2 to CTV 3 is currently supported.")
  }
  # Get mapping
  read2_ctv3 <- get_read2_ctv3_map(overwrite = overwrite)
  # Map codes of length 5+ only
  codes <- unique(codes[!is.na(codes)])
  in_codes <- data.table::data.table(READV2_CODE = substr(codes, 1, 5))
  in_codes <- in_codes[stringr::str_length(READV2_CODE) == 5]
  if (nrow(in_codes) != length(codes)) {
    message("Read v2 codes shorter than 5 digits have been dropped.")
  }
  out_codes <- merge(in_codes, read2_ctv3, by = "READV2_CODE")
  out_codes <- out_codes[, list(read_2 = READV2_CODE,
                                read_3 = READV3_CODE,
                                desc = TERMV3_DESC,
                                TERMV3_TYPE)]
  out_codes <- unique(out_codes)
  # Select preferred description where multiple terms are present
  out_codes <- out_codes[order(read_3, TERMV3_TYPE, desc)]
  out_codes <- out_codes[,
                         utils::head(.SD, 1),
                         by = read_3, .SDcols = c("read_2", "desc")]
  out_codes <- out_codes[, list(read_2, read_3, desc)][order(read_2, read_3)]
  out_codes[]
}
