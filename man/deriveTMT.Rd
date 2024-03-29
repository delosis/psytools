% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psytools_task_derivations.R
\name{deriveTMT}
\alias{deriveTMT}
\title{Derive TMT data.}
\usage{
deriveTMT(df)
}
\arguments{
\item{df}{Data frame with TMT data, read from CSV file exported from Delosis server.}
}
\value{
Derived data frame with summaries.
}
\description{
Applies to the Trail Making Test
(part of the \href{https://doi.org/10.1016/j.jneumeth.2013.10.024}{PEBL} battery))
implemented in Psytools.
}
\details{
Drop the initial practice.
}
