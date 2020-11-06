#' Detect and add replacement value for univariate time series.
#'
#' Requires `id`, `date` and `value` column
#'
#' @param data data.table object
#' @param value_col value column
#' @param id_col id column
#' @param date_col date column
#' @param frequency time series frequency per qear, e.g. 12 for months
#'
#' @import data.table
#'
#' @return data.table object with columns "Outlier" and "type" added for true
#'  values and replacement suggests
#' @export
#'
#' @examples
#' \dontrun{
#'   detect_outlier(fcf::dow30[ticker == "AAPL",], value_col = "ebit")[]
#' }
detect_outlier <- function(
  data,
  value_col,
  id_col = "ticker",
  date_col = "date",
  frequency = 4
) {

  index <- ticker <- Outlier <- replacements <- type <- NULL

  # Generate replacement data.table
  data_replacements <- data[, forecast::tsoutliers(stats::ts(
    get(value_col),
    frequency = frequency,
    start = c(year(min(get(date_col))), quarter(min(get(date_col)))),
    end = c(year(max(get(date_col))), quarter(max(get(date_col))))
  )), by = id_col]

  data[, index := 1:.N, by = id_col]
  data_new <- data_replacements[data, on = .(ticker, index)]
  data_new[, Outlier := fifelse(is.na(replacements), "no", "yes")]

  data_new <- melt(
    data_new,
    id.vars = c(id_col, date_col, "Outlier"),
    measure.vars = c(value_col, "replacements"),
    variable.name = "type",
    value.name = value_col
  )[!is.na(get(value_col))]

  # Replace new replacement values with "replacement" instead of yes/no in Outlier
  data_new[type == "replacements", Outlier := "replacement"]

  return(data_new)
}
