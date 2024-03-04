#' Perform PCA and Generate PCA Plot with Interaction
#'
#' This function performs Principal Component Analysis (PCA) on the input data matrix
#' and generates a PCA plot with interaction using the ggplot2 and factoextra packages.
#'
#' @param data A data frame containing the input data, where the variables of interest are columns 6 and onwards.
#' @param categories A character vector specifying the names of the categorical variables used for interaction coloring.
#'
#' @return None. The function generates the PCA plot with interaction.
#'
#' @rawNamespace import(ggplot2, except = last_plot)
#' @importFrom factoextra fviz_pca_ind
#' @importFrom stats prcomp
#'
#' @examples
#' # Example usage
#' dat <- MDAW::table_data |>
#'     dplyr::select(-c(1:5))
#' cat <- MDAW::table_data |>
#'     dplyr::select(c(4:5))
#' # pca_interaction_plot(data = dat, categories = cat)
#'
#' @export
#' @seealso \code{\link{prcomp}}, \code{\link{fviz_pca_ind}}, \code{\link{ggplot2}}, \code{\link{theme_minimal}}
#'
pca_interaction_plot <- function(data, categories) {
  # Step 1: Select relevant columns and impute missing values
  imputed_data <- data |>
    as.matrix() |>
    impute::impute.knn()

  # Step 2: Perform PCA
  pca_result <- stats::prcomp(imputed_data$data, scale. = TRUE)

  # Step 3: Plot the PCA results with interaction
  pca_plot <- factoextra::fviz_pca_ind(pca_result, geom = "point",
                            # Color by interaction of specified categories
                            habillage = categories,
                            addEllipses = TRUE) +
                            ggplot2::theme_bw() +
                            ggplot2::ggtitle("Interaction PCA Plot")
  pca_plot

}
