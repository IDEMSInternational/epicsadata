% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sum_rain_definitions.R
\name{sum_rain_definitions}
\alias{sum_rain_definitions}
\title{Summarize Rain Definitions}
\usage{
sum_rain_definitions(
  time = "annual_rain",
  total_rain,
  n_rain,
  sum_rain,
  n_raindays,
  data = NULL
)
}
\arguments{
\item{time}{A character string specifying the time period for which the rain definitions are summarised (\code{"annual_rain"} or \verb{"seasonal_rain""}).}

\item{total_rain}{Logical indicating whether total rain is considered.}

\item{n_rain}{Logical indicating whether the number of rainy days is considered.}

\item{sum_rain}{Numeric vector containing the sum of rainfall.}

\item{n_raindays}{Numeric vector containing the number of rainy days.}

\item{data}{Optional additional data (default \code{NULL}).}
}
\value{
A list containing summarised rain definitions for the specified time period.
}
\description{
This function summarises rain definitions for a specific time period.
}
\examples{
# TODO
}
