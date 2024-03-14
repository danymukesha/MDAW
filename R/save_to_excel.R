#' Save a tibble to an XLSX file
#'
#' [`save_to_xlsx`] saves a tibble (data frame) to an XLSX file
#' using the writexl package.
#'
#' @param data A tibble to be saved.
#' @param path The file path where the XLSX file will be saved.
#'
#' @return Nothing is returned. The tibble is saved to the specified path.
#'
#' @importFrom writexl write_xlsx
#'
#' @example man/examples/save_excel_example.R
#'
#' @export
save_to_xlsx <- function(data, path) {
  writexl::write_xlsx(data, path)
}
