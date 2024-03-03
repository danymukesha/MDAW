#' Clean metabolites from Table of Peaks
#'
#' `clean_table_peak` filters clean metabolites from the provided
#' table_peak based on criteria, such as missing values and
#' standard deviation of peaks.
#'
#' @param table_peak The data frame containing metabolite
#' information including QC_min_QC_RSD and min_Perc_missing.
#' @param min_QC_RSD The Relative Standard Deviation
#' @param min_Perc_missing The percentage of absent/missing values
#' @return A data frame containing only clean metabolites.
#' @examples
#' library(MDAW)
#' peaks <- MDAW::table_peak
#' clean_metabolites <-   MDAW::clean_table_peak(table = table_peak,
#'     min_QC_RSD = 20,
#'     min_Perc_missing = 20)
#'
#' @export
clean_table_peak <- function(table,
                             min_QC_RSD = NULL,
                             min_Perc_missing = NULL) {
    if ((!is.null(min_QC_RSD)) || (!is.null(min_Perc_missing))) {
        clean_table <- table |>
            dplyr::filter(QC_RSD < min_QC_RSD,
                          Perc_missing < min_Perc_missing)
        cat(nrow(clean_table), "remaining peaks.")
        return(clean_table)
    }
    cat(nrow(table), "remaining peaks.")
    return(table)
}
