% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind.R
\name{bind.points}
\alias{bind.points}
\alias{bind.points.data.frame}
\alias{bind.points.SpatialPointsDataFrame}
\title{Combine sets of points to form a single collection.}
\usage{
bind.points(...)

\method{bind.points}{data.frame}(...)

\method{bind.points}{SpatialPointsDataFrame}(...)
}
\arguments{
\item{...}{sets of points as either data frames or SpatialPointsDataFrames.}
}
\value{
Points in same format as given.
}
\description{
Combine sets of points to form a single collection.
}
