#' Create a binary Y vector for stratifying the samples
#'
#' This function takes a data table and creates a binary Y vector based on a specified column.
#' The column should correspond to the class labels, with two distinct groups.
#'
#' @param dataTable A data table containing the class labels.
#' @param classColumn The name of the column in the data table that corresponds to the class labels.
#' @return A binary vector where the class labels are converted into binary values (1 for one group, 0 for the other).
#'
#' @name create_binary_Y
#'
#' @export
#' @examples
#' dataTable <- data.frame(Class = c("GC", "HE", "GC", "HE"))
#' binaryY <- create_binary_Y(dataTable, "Class")
create_binary_Y <- function(dataTable, classColumn) {
  # Extract the column corresponding to class labels
  outcomes <- dataTable[[classColumn]]

  # Convert class labels into binary (GC = 1, HE = 0)
  Y <- ifelse(outcomes == "GC", 1, 0)

  return(Y)
}
