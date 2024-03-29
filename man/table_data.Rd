% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{table_data}
\alias{table_data}
\title{Metabolite `Data` with the Interactive Table}
\format{
`table_data` This table encompasses 140 samples and 149 metabolites;
in a data frame with 140 rows and 149 columns:
\describe{
  \item{RowID}{The `RowID` column represents row id of the sample}
  \item{Idx}{The `idX` column represents index of the sample}
  \item{Day of Expt}{The `Day of Expt` column represents data of extraction
  for the sample}
  \item{SampleType}{The `SampleType` column specifies whether
  the sample pertains to a pooled quality control (QC) sample
  or a study sample.}
  \item{QC}{The `QC` column identified the Quality Control sample \cr
  (if QC==1 the sample is for quality control, if QC is 0
  the the sample is not a quality control.)}
  \item{Class}{The `Class` column denotes the clinical outcome
   observed for each individual, with abbreviations representing
    different conditions: "GC" for Gastric Cancer, "BN" for Benign Tumor,
     and "HE" for Healthy Control.}
}
}
\source{
\url{https://www.metabolomicsworkbench.org/data/DRCCMetadata.php?Mode=Project&ProjectID=PR000699}
}
\usage{
table_data
}
\description{
Each row within the table_data corresponds to a distinct urine sample,
with the following column descriptions:
}
\keyword{datasets}
