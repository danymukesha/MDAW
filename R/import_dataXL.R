#' Import Data and Peak tables from xslx
#'
#' [import_dataXL()] reads sheets from xslx and load them.
#'
#' @param xlsx_file_name A name of excel file.
#' @param sheet_name A name of indicating the sheet of interest.
#' @param interactive_output Boolean indicating whether to output
#' the table in interactive mode using reactable.
#'
#' @return  The table of the `tibble` format or as a reactable
#' if interactive_output is *TRUE*.
#'
#' @examples
#' library(MDAW)
#' excel_file <- "Gastric_NMR.xlsx"
#' path <- system.file("extdata", "Gastric_NMR.xlsx", package = "MDAW")
#' MDAW::import_dataXL(xlsx_file_name = path,
#'     sheet_name = "data")
#'
#' @name import_dataXL
#'
#' @export
import_dataXL <- function(xlsx_file_name, sheet_name = NULL, interactive_output = FALSE) {
    if (!file.exists(xlsx_file_name)) {
        stop(paste(xlsx_file_name, "does not exist."))
    }

    if (!grepl("\\.xlsx$", xlsx_file_name)) {
        stop(paste(xlsx_file_name, "should be a .xlsx file."))
    }

    check_sheet_name <- function(file){

            xlsx_sheets <- readxl::excel_sheets(path = file)
            msg <- paste0("The excel file '", file,
                   "' contains the following sheet names:", "\n",
                   sep = "") |>
                as.character()
            cat(msg)

            cat(as.character(xlsx_sheets),
                fill = 2,
                labels = paste0("[", seq(length(xlsx_sheets)), "]:"))
            sheet_name <-
                readline(prompt = "Please Type here which sheet name you want: ")
            return(sheet_name)
        }

    if (is.null(sheet_name)) {
        sheet_name <- check_sheet_name(file = xlsx_file_name)
    }

    cat("Loading sheet:", sheet_name, "\n")
    data <- readxl::read_excel(xlsx_file_name, sheet = sheet_name)

    data[data == -99] <- NA

    data <- tibble::rownames_to_column(data, var = "RowID")

    cat("TOTAL ROWS:", nrow(data), "\n")
    cat("Done!\n")

    if(interactive_output) {
        return(reactable::reactable(data,
            defaultSorted = colnames(data[1]),
            searchable = TRUE, minRows = 10,
            paginationType = "simple"
            ))
    } else {
        return(data)
    }
}
