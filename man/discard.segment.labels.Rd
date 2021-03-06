% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics.R
\name{discard.segment.labels}
\alias{discard.segment.labels}
\title{Discard overlapping segment value labels.}
\usage{
discard.segment.labels(segments, name, radius)
}
\arguments{
\item{segments}{a data frame of segments.}

\item{name}{name of column holding value in \code{segments}.}

\item{radius}{distance (in coordinate units) within to clear of overlaps.}
}
\value{
A subset of \code{segments}.
}
\description{
Remove overlapping labels so that within given radius only one point
with the greatest value is kept.
}
