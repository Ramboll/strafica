% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sp.R
\name{sp.to.lines}
\alias{sp.to.lines}
\title{Convert \pkg{sp} lines to its data frame equivalent.}
\usage{
sp.to.lines(shapes, fun = identity, data.only = FALSE, verbose = TRUE)
}
\arguments{
\item{shapes}{a \code{SpatialLinesDataFrame} object.}

\item{fun}{a function to apply to coordinates, e.g. \code{\link{round}}.}

\item{data.only}{if \code{TRUE}, don't parse geometry, return only data.}

\item{verbose}{a logical indicating whether or not to print out progress.}
}
\value{
A list with two data frames: data and lines.
}
\description{
Convert \pkg{sp} lines to its data frame equivalent.
}
