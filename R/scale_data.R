#' Scales input data based on various methods
#'
#' Scales input data \code{x} based on various methods such as 'auto', 'pareto', 'vast', or 'level'.
#'
#' @param x An array-like object that contains the data.
#' @param axis An integer or NULL, (default 0). The axis along which to operate.
#' @param ddof An integer, (default 1). The degrees of freedom correction.
#' @param method A character string, (default "auto"). Method used to scale x. Accepted methods are 'auto', 'pareto', 'vast', and 'level'.
#' @param mu A numeric or "default", (default "default"). If mu is provided it is used, however, by default it is calculated.
#' @param sigma A numeric or "default",  (default "default"). If sigma is provided it is used, however, by default it is calculated.
#' @param return_mu_sigma A logical, (default FALSE). If \code{TRUE}, mu and sigma are returned instead of z. Note, this is useful if mu and sigma want to be stored for future use.
#'
#' @return If \code{return_mu_sigma = FALSE}, returns the scaled data z. If \code{return_mu_sigma = TRUE}, returns a list containing the scaled data z, mu, and sigma.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' scaled_data <- scale(x)
#'
#' # Define a 2-dimensional input matrix
#' x <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
#' # Scale the input matrix using the default settings
#' scaled_data <- scale(x)
#' # Print the scaled data
#' print(scaled_data)
#'
#' @export
#'
scale_data <- function(x, axis = 0, ddof = 1, method = "auto", mu = "default",
                  sigma = "default", return_mu_sigma = FALSE) {
  if (!is.double(x)) {
    stop("Input data x must be numeric.")
  }

  x <- x |> as.matrix()

  nrow_x <- ifelse(is.list(x), 1, nrow(x))

  x <- matrix(x, ncol = 1) # Ensure x is a matrix even if it's 1-dimensional

  if (axis == 1) {
    x <- t(x)
  }

  if (mu == "default") {
    mu <- colMeans(x, na.rm = TRUE)
  }
  if (sigma == "default") {
    sigma <- apply(x, 2, function(col) sd(col, na.rm = TRUE))
    sigma[sigma == 0] <- 1
  }

  if (length(mu) != ncol(x)) {
    stop("Length of mu array does not match x matrix.")
  }
  if (length(sigma) != ncol(x)) {
    stop("Length of sigma array does not match x matrix.")
  }

  if (method == "auto") {
    z <- sweep(x, 2, mu, "-") / sigma
  } else if (method == "pareto") {
    z <- sweep(x, 2, mu, "-") / sqrt(sigma)
  } else if (method == "vast") {
    z <- (sweep(x, 2, mu, "-") / sigma) * (mu / sigma)
  } else if (method == "level") {
    z <- sweep(x, 2, mu, "-") / mu
  } else {
    stop("Method has to be either 'auto', 'pareto', 'vast', or 'level'.")
  }

  if (axis == 1) {
    z <- t(z)
  }

  z <- matrix(z, ncol = nrow_x)

  if (return_mu_sigma) {
    return(list(z = z, mu = mu, sigma = sigma))
  } else {
    return(z)
  }

}
