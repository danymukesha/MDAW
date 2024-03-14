#' #' kNN missing value imputation using Euclidean distance.
#' #'
#' #' @param x An array-like object that contains the data with NaNs.
#' #' @param k Positive integer excluding 0, (default 3). The number of nearest neighbours to use.
#' #' @return An array-like object corresponding to x with NaNs imputed.
#' #'
#' #' @import TraMineR
#' #' @export
#' #' @examples
#' #' do
#' #' # Example 1: Impute NaNs in a larger matrix
#' #' data <- matrix(c(1, 2, NaN, 4, NaN, 6, 7, 8, 9, NaN, 11, NaN), nrow = 3)
#' #' imputed_data <- knnimpute(data)
#' #' print(imputed_data)
#' #'
#' #' # Example 2: Impute NaNs in a larger data frame
#' #' data <- data.frame(A = c(1, 2, NaN, 4, 5), B = c(6, NaN, 8, 9, 10), C = c(NaN, 12, 13, 14, 15))
#' #' imputed_data <- knnimpute(as.matrix(data))
#' #' print(imputed_data)
#' knnimpute <- function(x, k=3) {
#'   # Error check for k value
#'   if (!is.double(k)) {
#'     stop("k is not an integer")
#'   }
#'   if (k < 1) {
#'     stop("k must be greater than zero")
#'   }
#'   k_max <- ncol(x) - 1
#'   if (k_max < k) {
#'     stop(sprintf("k value is too high. Max k value is %d", k_max))
#'   }
#'
#'   # z is the returned array with NaNs imputed
#'   z <- t(x)
#'
#'   # Use columns without NaNs for knnimpute
#'   nan_check <- is.na(x)
#'   no_nan <- ifelse(colSums(nan_check) == 0, 1, 0)
#'
#'   # Error check that not all columns have NaNs
#'   x_no_nan <- x[, no_nan == 1]
#'   if (sum(x_no_nan, na.rm = TRUE) == 0) {
#'     stop("All colummns of the input data contain missing values. Unable to impute missing values.")
#'   }
#'
#'   # Calculate pairwise distances between columns, and covert to square-form distance matrix
#'   pair_dist <- dist(t(x_no_nan), method = "euclidean")
#'   sq_dist <- as.matrix(TraMineR:::dist2matrix(pair_dist))
#'
#'   # Make diagonals negative and sort
#'   dist <- t(apply(sq_dist - diag(nrow(sq_dist)), 2, sort))
#'   dist_idx <- t(apply(sq_dist - diag(nrow(sq_dist)), 2, order))
#'
#'   # Find where neighbours are equal distance
#'   equal_dist_a <- t(apply(dist[2:nrow(dist), ], 1, diff)) == 0
#'   equal_dist_b <- rep(0, nrow(dist))
#'   equal_dist <- rbind(equal_dist_a, equal_dist_b)
#'
#'   # Get rows and cols for missing values
#'   nan_idx <- which(is.na(x), arr.ind = TRUE)
#'   nan_rows <- nan_idx[, 1]
#'   nan_cols <- nan_idx[, 2]
#'
#'   # Impute each NaN value
#'   for (i in 1:length(nan_rows)) {
#'     # Error check for rows with all NaNs
#'     if (all(is.na(x[nan_rows[i], ]))) {
#'       warning(sprintf("Row %d contains all NaNs, so Row %d is imputed with zeros.", nan_rows[i], nan_rows[i]))
#'     }
#'
#'     # Create a loop from 1 to len(dist_idx) - k
#'     lastk <- nrow(dist_idx) - k
#'     loopk <- c(1)
#'     while (lastk > loopk[length(loopk)]) {
#'       loopk <- c(loopk, loopk[length(loopk)] + 1)
#'     }
#'
#'     # Impute
#'     for (j in loopk) {
#'       L_a <- equal_dist[j + k - 1, nan_cols[i]]
#'       L <- which(L_a == 0)[1]  # equal_dist neighbours
#'
#'       x_vals_r <- nan_rows[i]
#'       x_vals_c <- dist_idx[j:j + k + L, nan_cols[i]]
#'       x_vals <- x[x_vals_r, x_vals_c]
#'       weights <- 1 / dist[1:(k + L), nan_cols[i]]
#'       imp_val <- wmean(x_vals, weights)  # imputed value
#'       if (!is.na(imp_val)) {
#'         z[nan_rows[i], nan_cols[i]] <- imp_val
#'         break
#'       }
#'     }
#'   }
#'
#'   # Transpose z
#'   z <- t(z)
#'   return(z)
#' }
#'
#' #' Weighted Mean Function
#' #'
#' #' @param x Values
#' #' @param w Weights
#' #' @return Weighted mean
#' wmean <- function(x, w) {
#'   sum(x * w) / sum(w)
#' }
