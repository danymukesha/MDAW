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
#' @examples
#' library(tibble)
#' data <- tibble(x = 1:5, y = letters[1:5])
#' save_to_xlsx(data, "data.xlsx")
#'
#' @export
save_to_xlsx <- function(data, path) {
  writexl::write_xlsx(data, path)
}
