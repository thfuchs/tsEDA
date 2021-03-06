% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect_outlier.R
\name{detect_outlier}
\alias{detect_outlier}
\title{Detect and add replacement value for univariate time series.}
\usage{
detect_outlier(
  data,
  value_col,
  id_col = "ticker",
  date_col = "date",
  frequency = 4
)
}
\arguments{
\item{data}{data.table object}

\item{value_col}{value column}

\item{id_col}{id column}

\item{date_col}{date column}

\item{frequency}{time series frequency per qear, e.g. 12 for months}
}
\value{
data.table object with columns "Outlier" and "type" added for true
values and replacement suggests
}
\description{
Requires \code{id}, \code{date} and \code{value} column
}
\examples{
\dontrun{
  detect_outlier(fcf::dow30[ticker == "AAPL",], value_col = "ebit")[]
}
}
