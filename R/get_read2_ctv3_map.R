#' Get Read v2 to CTV3 mapping from UK Biobank
#'
#' @description Internal function. Downloads the mapping files from https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=592
#'   and extracts the "read_v2_read_ctv3" sheet from the \code{.xlsx} file in
#'   the downloaded archive.
#'
#' @param overwrite Overwrite existing file (default FALSE).
#' @param extract_file Specify the name of the .xlsx file to extract from the
#'   archive (optional).
#'
#' @return Data table with the Read v2 to CTV3 mapping provided by UK Biobank.
#'
#' @keywords internal
#' @noRd
#'
get_read2_ctv3_map <- function(overwrite = FALSE, extract_file = NULL) {
  READV2_CODE = READV3_CODE = READV2_DESC = TERMV3_DESC = NULL
  url <- "https://biobank.ndph.ox.ac.uk/showcase/ukb/auxdata/primarycare_codings.zip"
  path <- "map_all"
  map_path <- "map_read2_ctv3"
  if (file.exists(map_path) & overwrite == FALSE) {
    # Open mapping
    mapping <- fread(map_path)
  } else {
    # Get mapping
    get_mapping(overwrite)
    # Identify .xlsx file if not provided
    if (is.null(extract_file)) {
      mapping_files <- utils::unzip(path, list = TRUE)
      extract_file <- stringr::str_subset(mapping_files$Name, ".xlsx")
      if (length(extract_file) > 1) {
        stop("Cannot identify .xlsx file with mapping in the downloaded zip file. Specify file name with the extract_file argument.")
      }
    }
    # Unzip to temporary location
    xlsx_temp <- tempfile()
    utils::unzip(path, files = extract_file, exdir = xlsx_temp)
    xlsx_path <- file.path(xlsx_temp, extract_file)
    # Extract Read v2 to CTV3 mapping
    mapping <- readxl::read_xlsx(xlsx_path,
                                 sheet = "read_v2_read_ctv3",
                                 progress = FALSE)
    mapping <- data.table::as.data.table(mapping)
    mapping <- mapping[!is.na(READV2_CODE) & !is.na(READV3_CODE)]
    # Impute missing TERMV3_DESC
    mapping[is.na(TERMV3_DESC) | TERMV3_DESC == "", TERMV3_DESC := READV2_DESC]
    # Save
    fwrite(mapping, map_path)
  }
  mapping[]
}
