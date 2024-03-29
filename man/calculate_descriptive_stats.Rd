% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_descriptive_stats.R
\name{calculate_descriptive_stats}
\alias{calculate_descriptive_stats}
\title{Perform descriptive statistics on metabolomics data}
\usage{
calculate_descriptive_stats(data)
}
\arguments{
\item{data}{A data frame containing metabolomics data. Each row represents a single urine sample, with columns M1...M149 describing metabolite concentrations, Column SampleType indicating whether the sample was a pooled QC or a study sample, and Column Class indicating the clinical outcome observed for that individual (GC = Gastric Cancer, BN = Benign Tumor, HE = Healthy Control).}
}
\value{
A data frame containing descriptive statistics for each metabolite across different sample types and classes.
}
\description{
This function calculates descriptive statistics for metabolomics data, including mean, median, standard deviation, minimum, maximum, and quartiles.
}
\examples{
data <- MDAW::table_data
descriptive_stats <- calculate_descriptive_stats(data)

}
