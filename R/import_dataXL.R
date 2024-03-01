#' Import Data and Peak tables from xslx
#'
#' [import_dataXL()] reads sheets from xslx and load them.
#'
#' @param xlsx_file_name A name of excel file.
#' @param sheet_name A name of indicating the sheet of interest.
#'
#' @return The table of the `tibble` format.
#'
#' @examples
#'
#' import_dataXL("data-raw/Gastric_NMR.xlsx", "data")
#'
#' @name import_dataXL
#'
#' @export
import_dataXL <- function(xlsx_file_name, sheet_name) {
  if (!file.exists(xlsx_file_name)) {
    stop(paste(xlsx_file_name, "does not exist."))
  }

  if (!grepl("\\.xlsx$", xlsx_file_name)) {
    stop(paste(xlsx_file_name, "should be a .xlsx file."))
  }

  cat("Loading sheet:", sheet_name, "\n")
  data <- readxl::read_excel(xlsx_file_name, sheet = sheet_name)

  data[data == -99] <- NA

  data <- tibble::rownames_to_column(data, var = "RowID")

  cat("TOTAL ROWS:", nrow(data), "\n")
  cat("Done!\n")

  return(data)
}
