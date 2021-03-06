% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind.R
\name{bind.polys}
\alias{bind.polys}
\alias{bind.polys.data.frame}
\alias{bind.polys.list}
\alias{bind.polys.SpatialPolygonsDataFrame}
\title{Combine sets of polygons to form a single collection.}
\usage{
bind.polys(...)

\method{bind.polys}{data.frame}(...)

\method{bind.polys}{list}(...)

\method{bind.polys}{SpatialPolygonsDataFrame}(...)
}
\arguments{
\item{...}{sets of polygons as either data frames, lists as
returned by \code{\link{sp.to.polys}} or SpatialPolygonsDataFrames.}
}
\value{
Polygons in same format as given.
}
\description{
Combine sets of polygons to form a single collection.
}
