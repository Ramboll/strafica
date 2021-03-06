% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{pclip}
\alias{pclip}
\title{Clip to limits.}
\usage{
pclip(x, xmin = -Inf, xmax = Inf, na.rm = FALSE)
}
\arguments{
\item{x}{a numeric object.}

\item{xmin}{minimum to clip to.}

\item{xmax}{maximum to clip to.}

\item{na.rm}{\code{TRUE} to remove missing values.}
}
\value{
A numeric object the size of \code{x}.
}
\description{
All elements smaller than \code{xmin} will be raised to \code{xmin} and
all elements greater than \code{xmax} will be lowered to \code{xmax}.
}
\seealso{
\code{\link{pmin}}, \code{\link{pmax}}
}
