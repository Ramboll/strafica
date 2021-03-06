% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/union.R
\name{union.polys}
\alias{union.polys}
\alias{union.polys.data.frame}
\alias{union.polys.list}
\alias{union.polys.SpatialPolygonsDataFrame}
\title{Combine sets of polygons to form one polygon.}
\usage{
union.polys(...)

\method{union.polys}{data.frame}(...)

\method{union.polys}{list}(...)

\method{union.polys}{SpatialPolygonsDataFrame}(...)
}
\arguments{
\item{...}{sets of polygons as either outline data frames, lists as
returned by \code{\link{sp.to.polys}} or SpatialPolygonsDataFrames.}
}
\value{
A polygon in same format as given.
}
\description{
Combine sets of polygons to form one polygon.
}
\seealso{
\code{\link{aggregate.polys}}
}
