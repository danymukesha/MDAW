library(MDAW)

# The table of Data from an Excel file(data sheet)
table_data <-
    MDAW::import_dataXL(xlsx_file_name = "inst/extdata/Gastric_NMR.xlsx",
        sheet_name = "Data")

# The table of Peak from an Excel file(peak sheet)
table_peak <-
    MDAW::import_dataXL(xlsx_file_name = "inst/extdata/Gastric_NMR.xlsx",
        sheet_name = "Peak")

usethis::use_data(table_data, overwrite = TRUE)
usethis::use_data(table_peak, overwrite = TRUE)

