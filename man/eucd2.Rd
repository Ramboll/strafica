% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist.R
\name{eucd2}
\alias{eucd2}
\title{Squared euclidean distance.}
\usage{
eucd2(x1, y1, x2, y2)
}
\arguments{
\item{x1}{x-coordinates of first points.}

\item{y1}{y-coordinates of first points.}

\item{x2}{x-coordinates of second points.}

\item{y2}{y-coordinates of second points.}
}
\value{
A numeric object.
}
\description{
Omitting the square root makes this much faster to calculate than \code{eucd}.
While this is not the actual euclidean distance, the square is sufficient for
some cases, e.g. finding the nearest neighbour.
}
\seealso{
\code{\link{eucd}}
}
