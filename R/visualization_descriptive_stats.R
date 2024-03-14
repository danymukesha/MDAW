#' Visualize metabolite concentrations by sample type
#'
#' This function creates boxplots to visualize the distribution of metabolite concentrations by sample type.
#'
#' @param data A data frame containing metabolomics data.
#'
#' @return A ggplot object displaying boxplots of metabolite concentrations by sample type.
#'
#' @examples
#' data <- MDAW::table_data
#' plot_metabolite_boxplots(data)
#'
#' @import ggplot2
#' @import reshape2
#' @importFrom rlang .data
#' @export
plot_metabolite_boxplots <- function(data) {
  # Select metabolite concentration columns and SampleType column
  metabolite_data <- data[, grepl("^M", names(data))]
  sample_type <- data$SampleType

  # Melt the data for plotting
  melted_data <- reshape2::melt(cbind(metabolite_data,
                                      SampleType = sample_type),
                                id.vars = "SampleType")

  # Create boxplot
  p <- ggplot(melted_data, aes(x = .data$variable,
                               y = .data$value,
                               fill = .data$SampleType)) +
    geom_boxplot() +
    labs(x = "Metabolite",
         y = "Concentration",
         title = "Metabolite Concentrations by Sample Type") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(p)
}

#' Visualize metabolite concentrations by class
#'
#' This function creates boxplots to visualize the distribution of metabolite concentrations by class.
#'
#' @param data A data frame containing metabolomics data.
#'
#' @return A ggplot object displaying boxplots of metabolite concentrations by class.
#'
#' @examples
#' data <- MDAW::table_data
#' plot_metabolite_boxplots_by_class(data)
#'
#' @import ggplot2
#' @export
plot_metabolite_boxplots_by_class <- function(data) {
  # Select metabolite concentration columns and Class column
  metabolite_data <- data[, grepl("^M", names(data))]
  class <- data$Class

  # Melt the data for plotting
  melted_data <- reshape2::melt(cbind(metabolite_data, Class = class), id.vars = "Class")

  # Create boxplot
  p <- ggplot(melted_data, aes(x = .data$variable,
                               y = .data$value,
                               fill = .data$Class)) +
    geom_boxplot() +
    labs(x = "Metabolite",
         y = "Concentration",
         title = "Metabolite Concentrations by Class") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(p)
}

#' Visualize distribution of metabolite concentrations
#'
#' This function creates histograms to visualize the distribution of metabolite concentrations.
#'
#' @param data A data frame containing metabolomics data.
#' @param metabolite_name Name of the metabolite to visualize.
#'
#' @return A ggplot object displaying histogram of metabolite concentrations.
#'
#' @examples
#' data <- MDAW::table_data
#' plot_metabolite_histogram(data, "M1")
#'
#' @import ggplot2
#' @export
plot_metabolite_histogram <- function(data, metabolite_name) {
  # Select metabolite concentration column
  metabolite_data <- data[, metabolite_name]

  # Create histogram
  p <- ggplot(data, aes(x = !!rlang::sym(metabolite_name))) +
    geom_histogram(bins = 20, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(x = "Concentration", y = "Frequency", title = paste("Histogram of", metabolite_name, "Concentrations")) +
    theme_minimal()

  return(p)
}
