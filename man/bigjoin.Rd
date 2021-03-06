% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{bigjoin}
\alias{bigjoin}
\title{Join two data frames together.}
\usage{
bigjoin(x, y, by = intersect(names(x), names(y)), size = -1)
}
\arguments{
\item{x}{a data frame.}

\item{y}{a data frame.}

\item{by}{names of columns to join by.}

\item{size}{ignored.}
}
\value{
A data frame with as many rows as \code{x}.
}
\description{
Join two data frames together.
}
