% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/webmap.R
\name{webmap.ynum2deg}
\alias{webmap.ynum2deg}
\title{Convert webmap Y-tile number to WGS 84 Y-coordinate.}
\usage{
webmap.ynum2deg(ytile, zoom)
}
\arguments{
\item{ytile}{Y-tile number.}

\item{zoom}{zoom level.}
}
\value{
WGS 84 Y-coordinate as numeric.
}
\description{
Convert webmap Y-tile number to WGS 84 Y-coordinate.
}
\references{
\url{http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}
}
