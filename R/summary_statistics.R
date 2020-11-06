#' Quick statistical overview of your data
#'
#' @param data data.frame
#' @param cols vector of column names to generate statistics for
#'
#' @import data.table
#'
#' @return data.frame object with mean, mean, sd, min, max and N
#' @export
summary_statistics <- function(data, cols) {

  data[, purrr::map_df(.SD, function(x) c(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    `Std. Dev` = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    N = sum(!is.na(x))
  ), .id = "Variable"), .SDcols = cols]

}
