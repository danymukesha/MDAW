#' Custom Train-Test Split Function
#'
#' This function splits the dataset and binary outcomes into training and test sets.
#' It replicates the functionality of scikit-learn's train_test_split function.
#'
#' @param dataTable2 The dataset to be split.
#' @param Y List of binary outcomes for stratification.
#' @param test_size Proportion of the dataset to include in the test set.
#' @param seed Seed for random number generation.
#' @return A list containing:
#'   \item{dataTrain}{Training dataset (dataframe)}
#'   \item{dataTest}{Test dataset (dataframe)}
#'   \item{Ytrain}{Known outcomes for the training dataset}
#'   \item{Ytest}{Known outcomes for the test dataset}
#'
#' @import caret
#' @examples
#' dataTable2 <- data.frame(matrix(rnorm(100*10), nrow=100, ncol=10))
#' Y <- sample(c(0,1), 100, replace = TRUE)
#' split_data <- custom_train_test_split(dataTable2, Y, test_size = 0.25, seed = 123)
#' dim(split_data$dataTrain)
#' dim(split_data$dataTest)
#' @export
custom_train_test_split <- function(dataTable2, Y, test_size = 0.25, seed = NULL) {
  set.seed(seed)
  indices <- caret::createDataPartition(Y, times = 1, p = 1 - test_size, list = FALSE)
  dataTrain <- dataTable2[indices, ]
  dataTest <- dataTable2[-indices, ]
  Ytrain <- Y[indices]
  Ytest <- Y[-indices]

  cat("DataTrain =", nrow(dataTrain), "samples with", sum(Ytrain), "positive cases.\n")
  cat("DataTest =", nrow(dataTest), "samples with", sum(Ytest), "positive cases.\n")

  return(list(dataTrain = dataTrain, dataTest = dataTest, Ytrain = Ytrain, Ytest = Ytest))
}
