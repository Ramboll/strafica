% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{dedup}
\alias{dedup}
\title{Remove duplicate rows from a data frame.}
\usage{
dedup(x, ...)
}
\arguments{
\item{x}{a data frame.}

\item{...}{names of columns to judge duplication by as either
symbols or strings. If not given, all columns are used.}
}
\value{
\code{x} with some rows removed.
}
\description{
Remove duplicate rows from a data frame.
}
