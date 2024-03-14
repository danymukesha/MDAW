library(tibble)
library(MDAW)

data <- tibble(x = 1:5, y = letters[1:5])
data

# MDAW::save_to_xlsx(data, "data.xlsx")
