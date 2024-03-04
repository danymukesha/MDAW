#' Univariate Statistics for Two-Class Data
#'
#' Calculates univariate statistics for two-class data including mean, median,
#' confidence intervals, and statistical tests such as t-test
#' or Mann-Whitney U test.
#'
#' @param DataTable Data sheet with the required columns.
#' @param PeakTable Peak sheet with the required columns.
#' @param group Name of the column in the DataTable that contains
#' the Class data.
#' @param posclass Name of the positive class in the group column.
#' @param parametric If \code{TRUE}, parametric statistics (mean, t-test)
#' are calculated. If \code{FALSE}, non-parametric statistics
#' (median, Mann-Whitney U test) are calculated. i.e. Median 95\%CI
#' and Mann-Whitney U test is calculated instead of Mean 95\%CI and t-test.
#' @param seed Used to seed the generator for  the Median 95\% CI bootstrap
#' (resample with replacement) when calculating bootstrap confidence intervals.
#' Ignored if \code{parametric} is \code{TRUE}.
#' Note, if parametric=TRUE the seed is irrelevant as Mean 95\% CI
#' is calculated parametrically.
#'
#' @return A data frame containing univariate statistics for each peak.
#' The table  should contain multiple univariate statistics (2 class).
#'
#' @export
#'
#' @examples
#' # Example usage
#' DataTable <- MDAW::table_data
#' PeakTable <- MDAW::table_peak
#' table_data_2Class <- DataTable |>
#'       dplyr::filter(Class == c("HE", "GC"))
#' # Reduce data table only to GC and HE class members
#' pos_outcome <- "GC"
#' univariate_2class(table_data_2Class, PeakTable, group = "Class",
#'       posclass = pos_outcome)
#'
#' @importFrom dplyr filter mutate select
#' @importFrom stats median quantile t.test wilcox.test sd shapiro.test
#' @importFrom stats p.adjust
#' @importFrom utils packageVersion
#'
univariate_2class <- function(DataTable,
                              PeakTable,
                              group,
                              posclass,
                              parametric = TRUE,
                              seed = NULL) {

  # Error checks
  if (!group %in% colnames(DataTable)) {
    stop(paste("Column '", group, "' does not exist in DataTable", sep = ""))
  }
  if (!posclass %in% unique(DataTable[[group]])) {
    stop(paste("Positive class '", posclass, "' was not found in '", group, "' column.", sep = ""))
  }
  if (length(unique(DataTable[[group]])) != 2) {
    stop(paste("Column '", group, "' should have exactly 2 groups", sep = ""))
  }

  # Get x0, x1
  peaklist <- PeakTable$Name
  x <- DataTable[, peaklist]
  y <- DataTable[[group]]
  x1 <- x[y == posclass, ]  # x1 refers to posclass
  x0 <- x[y != posclass, ]

  # Create StatsTable
  StatsTable <- data.frame(
    Idx = PeakTable$Idx,
    Name = PeakTable$Name,
    Label = PeakTable$Label
  )

  if (parametric) {
    # Calculate mean and std
    StatsTable$Grp0_Mean <- colMeans(x0, na.rm = TRUE)
    Mean095CI <- 1.96 * (apply(x0, 2, sd, na.rm = TRUE) / sqrt(colSums(!is.na(x0))))
    StatsTable$Grp0_Mean_95CI <- paste(round(StatsTable$Grp0_Mean - Mean095CI, 2), round(StatsTable$Grp0_Mean + Mean095CI, 2))
    StatsTable$Grp1_Mean <- colMeans(x1, na.rm = TRUE)
    Mean195CI <- 1.96 * (apply(x1, 2, sd, na.rm = TRUE) / sqrt(colSums(!is.na(x1))))
    StatsTable$Grp1_Mean_95CI <- paste(round(StatsTable$Grp1_Mean - Mean195CI, 2), round(StatsTable$Grp1_Mean + Mean195CI, 2))

    # T-test
    ttest_result <- apply(x, 2, function(col) t.test(col ~ y)$p.value)
    StatsTable$TTestStat <- apply(x, 2, function(col) t.test(col ~ y)$statistic)
    StatsTable$TTestPvalue <- ttest_result

    # BH correction
    bonferroni_result <- p.adjust(ttest_result, method = "BH")
    StatsTable$bhQvalue <- bonferroni_result
  } else {
    # Calculate median and bootstrap CI
    StatsTable$Grp0_Median <- apply(x0, 2, median, na.rm = TRUE)
    StatsTable$Grp0_Median_95CI <- apply(x0, 2, function(col) {
      boot_vals <- replicate(100, median(sample(col, replace = TRUE)))
      c(
        round(quantile(boot_vals, 0.025), 2),
        round(quantile(boot_vals, 0.975), 2)
      )
    })

    StatsTable$Grp1_Median <- apply(x1, 2, median, na.rm = TRUE)
    StatsTable$Grp1_Median_95CI <- apply(x1, 2, function(col) {
      boot_vals <- replicate(100, median(sample(col, replace = TRUE)))
      c(
        round(quantile(boot_vals, 0.025), 2),
        round(quantile(boot_vals, 0.975), 2)
      )
    })

    # Mann-Whitney U
    mwu_result <- apply(x, 2, function(col) wilcox.test(col ~ y)$p.value)
    StatsTable$MannWhitneyU <- apply(x, 2, function(col) wilcox.test(col ~ y)$statistic)
    StatsTable$MannWhitneyPvalue <- mwu_result

    # BH correction
    bonferroni_result <- p.adjust(mwu_result, method = "BH")
    StatsTable$bhQvalue <- bonferroni_result
  }

  # Calculate total missing and total missing %
  nannum <- colSums(is.na(x))
  nanperc <- nannum / nrow(x)
  StatsTable$TotalMissing <- nannum
  StatsTable$PercTotalMissing <- round(nanperc * 100, 3)

  # Calculating missing % for group 0, and group 1
  nanperc_0 <- colSums(is.na(x0)) / nrow(x0)
  nanperc_1 <- colSums(is.na(x1)) / nrow(x1)
  StatsTable$Grp0_Missing <- round(nanperc_0 * 100, 3)
  StatsTable$Grp1_Missing <- round(nanperc_1 * 100, 3)

  # Shapiro-Wilk
  shapiro_result <- apply(x, 2, function(col) shapiro.test(col[!is.na(col)])$statistic)
  StatsTable$ShapiroW <- shapiro_result
  StatsTable$ShapiroPvalue <- apply(x, 2, function(col) shapiro.test(col[!is.na(col)])$p.value)

  # # Levenes
  # levene_result <- apply(x, 2, function(col) {
  #   newx0 <- x0[!is.na(x0[, col]), col]
  #   newx1 <- x1[!is.na(x1[, col]), col]
  #   levene.test(newx0, newx1)$statistic
  # })
  #
  # StatsTable$LeveneW <- levene_result
  # StatsTable$LevenePvalue <- apply(x, 2, function(col) {
  #   newx0 <- x0[!is.na(x0[, col]), col]
  #   newx1 <- x1[!is.na(x1[, col]), col]
  #   levene.test(newx0, newx1)$p.value
  # })

  return(StatsTable)
}
