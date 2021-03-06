% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{na.to.zero}
\alias{na.to.zero}
\alias{na.to.zero.data.frame}
\alias{na.to.zero.integer}
\alias{na.to.zero.numeric}
\title{Replace \code{NA}s with zeros.}
\usage{
na.to.zero(x, names = NULL)

\method{na.to.zero}{data.frame}(x, names = NULL)

\method{na.to.zero}{integer}(x, names = NULL)

\method{na.to.zero}{numeric}(x, names = NULL)
}
\arguments{
\item{x}{a numeric vector or a data frame.}

\item{names}{column names to replace in if \code{x} is a data frame.
If omitted, defaults to replacing in all numeric columns.}
}
\value{
\code{x} with zeros instead of \code{NA}s.
}
\description{
Replace \code{NA}s with zeros.
}
