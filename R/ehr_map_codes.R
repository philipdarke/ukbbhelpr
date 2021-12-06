#' Map EHR codes to another terminology
#'
#' Map EHR codes from one terminology to another using UK Biobank mapping
#' dictionaries. Currently supports mapping Read v2 to CTV 3 only.
#'
#' @param codes Vector of codes to map.
#' @param from Terminology to map from. Currently supports Read v2 codes only.
#' @param to Terminology to map to. Currently supports CTV3 only.
#' @param overwrite Overwrite existing mapping dictionary (default FALSE).
#'
#' @return Data table with mapped codes and description.
#'
#' @export
#'
ehr_map_codes <- function (codes, from = "read2", to = "ctv3", overwrite = FALSE) {
  code = READV2_CODE = TERMV2_ORDER = READV3_CODE = TERMV3_CODE = NULL
  TERMV3_DESC = TERMV3_TYPE = desc = NULL
  # Check arguments
  argument_check(codes, "character")
  argument_check(overwrite, "flag")
  if (from != "read2" | to != "ctv3") {
    stop("Only mapping from Read v2 to CTV 3 is currently supported.")
  }
  # Get mapping
  read2_ctv3 <- get_read2_ctv3_map(overwrite = overwrite)
  # Split Read v2 codes into codes/terms
  codes <- unique(codes[!is.na(codes)])
  out_codes <- data.table::data.table(code = codes)
  out_codes[, READV2_CODE := stringr::str_pad(code, width = 5, side = "right", pad = ".")]
  out_codes[, TERMV2_ORDER := "00"]
  out_codes[stringr::str_length(codes) == 7, TERMV2_ORDER := substr(code, 6, 7)]
  out_codes[, READV2_CODE := substr(READV2_CODE, 1, 5)]
  # Check all codes have been split
  out_codes <- out_codes[!is.na(READV2_CODE) & !is.na(TERMV2_ORDER)]
  if (nrow(out_codes) != length(codes)) {
    message(paste(length(codes) - nrow(out_codes), "codes could not be parsed."))
  }
  # Map codes
  out_codes <- merge(out_codes, read2_ctv3, by = c("READV2_CODE", "TERMV2_ORDER"))
  out_codes <- out_codes[, list(code,
                                read_2 = READV2_CODE,
                                term_2 = TERMV2_ORDER,
                                read_3 = READV3_CODE,
                                term_3 = TERMV3_CODE,
                                desc = TERMV3_DESC,
                                TERMV3_TYPE)]
  out_codes <- unique(out_codes)
  # Use first preferred description alphabetically where multiple preferred
  # terms are present for a CTV3 code e.g. XE2Pp
  out_codes <- out_codes[order(TERMV3_TYPE, desc)]
  out_codes <- out_codes[,
                         utils::head(.SD, 1),
                         by = c("read_2", "term_2")]
  # Return mapping table
  out_codes[, TERMV3_TYPE := NULL]
  data.table::setcolorder(out_codes, c(3, 1:2, 4:6))
  out_codes[]
}
