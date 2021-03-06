% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics.R
\name{aggregate.parallel.segments}
\alias{aggregate.parallel.segments}
\title{Aggregate values of equidirectional parallel segments.}
\usage{
\method{aggregate.parallel.segments}{not.an.s3.method}(segments, name, radius, angle, by = NULL)
}
\arguments{
\item{segments}{a data frame of segments.}

\item{name}{name of column holding value in \code{segments}.}

\item{radius}{distance (in coordinate units) within which
to find parellel segments.}

\item{angle}{maximum allowed angle difference (in degrees).}

\item{by}{columns in \code{segments} to parallelize over.
Each by-wise part will be aggregated independent of each other.}
}
\value{
Data frame \code{segments} with column "combined" added.
}
\description{
\code{aggregate.parallel.segments} detects which segments are equidirectional
and parallel within given tolerance (e.g. sidewalks on opposite sides
of a street) and will aggregate values on such segments, placing aggregate
values on both segments! For plotting, these duplicates (among other overlaps)
can be removed with \code{\link{discard.segment.labels}}.
}
