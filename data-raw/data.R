library(readxl)
library(tibble)
library(MDAW)

# The table of Data from an Excel file(data sheet)
table_data <-
  readxl::read_xlsx(path = "data-raw/Gastric_NMR.xlsx", sheet = "data") |>
  as.tibble()

# The table of Peak from an Excel file(peak sheet)
table_peak <-
  MDAW::

usethis::use_data(table_data, overwrite = TRUE)
usethis::use_data(table_peak, overwrite = TRUE)
