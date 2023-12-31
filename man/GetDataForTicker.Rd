% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_finance.R
\name{GetDataForTicker}
\alias{GetDataForTicker}
\title{Downloads data for each ticker.}
\usage{
GetDataForTicker(path_prefix, ticker, beg_date, end_date)
}
\arguments{
\item{path_prefix}{A path prefix denoting where to write the data files
to disk.}

\item{ticker}{A string describing a ticker.}

\item{beg_date}{A date describing when to start fetching data from.}

\item{end_date}{A date describing when to finish collecting data.}
}
\description{
Uses a try-catch to check if there's any sort of error when fetching the data
and writing to desk.
}
