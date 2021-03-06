% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clip.R
\name{clip.lines}
\alias{clip.lines}
\alias{clip.lines.data.frame}
\alias{clip.lines.list}
\alias{clip.lines.SpatialLinesDataFrame}
\title{Clip lines to a bounding polygon.}
\usage{
clip.lines(lines, ...)

\method{clip.lines}{data.frame}(lines, ...)

\method{clip.lines}{list}(lines, ...)

\method{clip.lines}{SpatialLinesDataFrame}(lines, ...)
}
\arguments{
\item{lines}{a data frame of lines or a list as returned by
\code{\link{sp.to.lines}} or a SpatialLinesDataFrame object.}

\item{...}{bounds either as a data frame of polygon outlines,
a SpatialPolygonsDataFrame object or bounding box coordinates
\code{(xmin=..., xmax=..., ymin=..., ymax=...)}.}
}
\value{
Lines in same format as \code{lines}.
}
\description{
Clip lines to a bounding polygon.
}
