#' #' KFold Cross-Validation with Binary Metrics Calculation
#' #'
#' #' Exhaustive search over param_dict calculating binary metrics.
#' #'
#' #' @param model An object assumed to store bootlist attributes in .model (e.g., modelPLS.model.x_scores_).
#' #' @param X Predictor variables, where n_samples is the number of samples and n_features is the number of predictors.
#' #' @param Y Response variables, where n_samples is the number of samples.
#' #' @param param_dict List of attributes to calculate and return bootstrap confidence intervals.
#' #' @param folds The number of folds used in the computation. Default is 5.
#' #' @param n_mc The number of Monte Carlo reps. Default is 1.
#' #' @param n_boot The number of bootstrap samples used in the computation for the plot. Default is 0.
#' #' @param n_cores Number of CPU cores to use. Default is -1 (auto).
#' #' @param ci Confidence interval. Default is 95.
#' #' @param stratify Logical indicating whether to stratify the folds. Default is TRUE.
#' #'
#' #' @import caret
#' #' @import parallel
#' #' @import doParallel
#' #' @import foreach
#' #' @import e1071
#' #'
#' #' @examples
#' #' library(e1071)
#' #' # Generate synthetic data
#' #' set.seed(123)
#' #' X <- matrix(rnorm(1000), ncol = 10)
#' #' Y <- sample(0:1, 100, replace = TRUE)
#' #'
#' #' # Define model function
#' #' svm_model <- function(X, Y) {
#' #'   e1071::svm(Y ~ ., data = as.data.frame(X), kernel = "radial")
#' #' }
#' #'
#' #' # Define parameters for cross-validation
#' #' param_dict <- list(kernel = c("linear", "radial", "polynomial"))
#' #'
#' #' # Perform k-fold cross-validation
#' #' results <- KFold(svm_model, X, Y, param_dict, folds = 5)
#' #'
#' #' # Print results
#' #' print(results)
#' #'
#' #' @return A list containing calculated statistics.
#' #' @export
#' KFold <- function(model, X, Y, param_dict, folds=5, n_mc=1, n_boot=0, n_cores=-1, ci=95, stratify=TRUE) {
#'
#'   # Initialize cluster for parallel processing
#'   if(n_cores == -1) {
#'     n_cores <- detectCores()
#'   }
#'   cl <- makeCluster(n_cores)
#'   registerDoParallel(cl)
#'
#'   # Set cross-validation index based on stratification
#'   if(stratify) {
#'     cv_idx <- createMultiFolds(Y, k = folds, times = 1)
#'   } else {
#'     cv_idx <- createFolds(Y, k = folds)
#'   }
#'
#'   # Perform cross-validation
#'   res <- foreach(i=1:folds, .combine=rbind) %dopar% {
#'     train_idx <- cv_idx[[i]]
#'     test_idx <- setdiff(1:length(Y), train_idx)
#'
#'     # Train model
#'     model_fit <- model(X[train_idx, ], Y[train_idx])
#'
#'     # Predict on test set
#'     y_pred <- predict(model_fit, newdata = X[test_idx, ])
#'
#'     # Calculate binary metrics
#'     TP <- sum(Y[test_idx] == 1 & y_pred == 1)
#'     TN <- sum(Y[test_idx] == 0 & y_pred == 0)
#'     FP <- sum(Y[test_idx] == 0 & y_pred == 1)
#'     FN <- sum(Y[test_idx] == 1 & y_pred == 0)
#'
#'     accuracy <- (TP + TN) / length(test_idx)
#'     precision <- TP / (TP + FP)
#'     recall <- TP / (TP + FN)
#'     f1_score <- 2 * precision * recall / (precision + recall)
#'
#'     metrics <- c(accuracy = accuracy, precision = precision, recall = recall, f1_score = f1_score)
#'
#'     # Add model parameters to metrics
#'     metrics$params <- param_dict
#'
#'     return(metrics)
#'   }
#'
#'   # Stop the cluster
#'   stopCluster(cl)
#'
#'   return(res)
#' }
