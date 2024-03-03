library(MDAW)

excel_file <- "Gastric_NMR.xlsx"
path <- system.file("extdata", "Gastric_NMR.xlsx", package = "MDAW")

# The table of Data from an Excel file(data sheet)
table_data <- MDAW::import_dataXL(xlsx_file_name = path, sheet_name = "data")
table_data

# The table of Peak from an Excel file(peak sheet)
table_peak <- MDAW::import_dataXL(xlsx_file_name = path, sheet_name = "peak")
table_peak
