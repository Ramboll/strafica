% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist.R
\name{point.line.dist}
\alias{point.line.dist}
\title{Distances of points from lines.}
\usage{
point.line.dist(p, a, b)
}
\arguments{
\item{p}{coordinates of points.}

\item{a}{coordinates of start points of lines.}

\item{b}{coordinates of end points of lines.}
}
\value{
A numeric vector of distances.
}
\description{
Either \code{p} or \code{a} and \code{b} can be vectorized, not both.
Coordinates should be given as either a numeric vector of length two
or a two-column matrix of X- and Y-coordinates.
}
\references{
\url{http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html}
}
