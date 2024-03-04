#' Split Data into Training and Test Sets
#'
#' This function takes a dataset and splits it into training and test sets
#' based on a specified ratio.
#'
#' @param data A data frame or matrix containing the dataset to be split.
#' @param split_ratio A numeric value between 0 and 1 indicating the proportion
#'  of the dataset to include in the training set. The rest will be included
#'  in the test set.
#' @param seed An optional seed for reproducibility.
#'
#' @return A list containing two elements: 'train' for the training set
#' and 'test' for the test set.
#'
#' @import caret
#'
#' @examples
#' data(iris)
#' split_data <- split_data_train_test(iris, split_ratio = 0.8, seed = 123)
#' dim(split_data$train)
#' dim(split_data$test)
#'
#' @export
split_data_train_test <- function(data, split_ratio, seed = NULL) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("This function requires the 'caret' package.
         Please install it using install.packages('caret').")
  }

  set.seed(seed)
  n <- nrow(data)
  train_indices <- caret::createDataPartition(y = NULL, times = 1,
                                              p = split_ratio, list = FALSE)
  train <- data[train_indices, ]
  test <- data[-train_indices, ]

  return(list(train = train, test = test))
}
