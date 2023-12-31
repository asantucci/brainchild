% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_finance.R
\name{ConvertRange}
\alias{ConvertRange}
\title{Strips away non-numeric characters and creates a column for Lo and Hi amount.}
\usage{
ConvertRange(range)
}
\arguments{
\item{amount}{A vector with entries of the form, "$beg - $end"}
}
\value{
A two column data.table describing the lo and high amount.
}
\description{
Strips away non-numeric characters and creates a column for Lo and Hi amount.
}
