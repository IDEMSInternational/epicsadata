% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_binary_file.R
\name{get_binary_file}
\alias{get_binary_file}
\title{Update PDF/JPEG Data}
\usage{
get_binary_file(country = c("mz", "zm"), station_id, type = c("pdf", "jpeg"))
}
\arguments{
\item{country}{A character vector specifying the country or countries from which to update the data. Options are \code{"mz"} and \code{"zm"}.}

\item{station_id}{A character string specifying the ID of the station for which to update the daily data.}

\item{type}{A character string specifying whether the data to retrieve is JPEG or PDF.}
}
\value{
This function does not return any value explicitly. It gets the PDF/JPEG data for the specified station in the specified country.
}
\description{
This function updates the PDF/JPEG data for a specific station in the specified country. It retrieves the data from Google Cloud Storage using the \code{get_data} function.
}
\details{
The \code{country} argument is a character vector that allows specifying one or more countries from which to get the PDF/JPEG data. The data will be updated for Mozambique (\code{"mz"}) and Zambia (\code{"zm"}). You can modify this argument to update data for different countries.
The \code{station_id} argument is a character string that specifies the ID of the station for which to update the PDF/JPEG data. The function will construct the filename by concatenating the \code{"pdf/"} or \verb{"jpeg"/} directory, the \code{station_id}, and the \code{file} extension. The filename will be passed to the \code{get_data} function to retrieve the data.
The function uses the invisible function to suppress the output of the \code{get_data} function, ensuring that the data retrieval process is not visible in the console.
}
\examples{
# get_binary_file("zm", "16", "pdf")
}
