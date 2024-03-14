#' Perform descriptive statistics on metabolomics data
#'
#' This function calculates descriptive statistics for metabolomics data, including mean, median, standard deviation, minimum, maximum, and quartiles.
#'
#' @param data A data frame containing metabolomics data. Each row represents a single urine sample, with columns M1...M149 describing metabolite concentrations, Column SampleType indicating whether the sample was a pooled QC or a study sample, and Column Class indicating the clinical outcome observed for that individual (GC = Gastric Cancer, BN = Benign Tumor, HE = Healthy Control).
#'
#' @return A data frame containing descriptive statistics for each metabolite across different sample types and classes.
#'
#' @examples
#' data <- MDAW::table_data
#' descriptive_stats <- calculate_descriptive_stats(data)
#'
#' @export
calculate_descriptive_stats <- function(data) {
  # Select only metabolite concentration columns
  metabolite_data <- data[, grepl("^M", names(data))]

  # Calculate descriptive statistics
  stats <- apply(metabolite_data, 2, function(x) {
    c(mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      Q1 = stats::quantile(x, 0.25, na.rm = TRUE),
      Q3 = stats::quantile(x, 0.75, na.rm = TRUE))
  })

  # Combine statistics into a data frame
  stats_df <- data.frame(t(stats))
  colnames(stats_df) <- c("Mean", "Median", "SD", "Min", "Max", "Q1", "Q3")

  return(stats_df)
}
