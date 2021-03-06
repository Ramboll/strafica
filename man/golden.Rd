% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{golden}
\alias{golden}
\title{Golden ratio dimensions.}
\usage{
golden(x)
}
\arguments{
\item{x}{a numeric vector of length one.}
}
\value{
A numeric vector of length two. Use \code{\link{min}} and
\code{\link{max}} to extract the wanted element.
}
\description{
Given one length, \code{golden} will return that length divided by and
multiplied by the golden ratio, i.e. length of the other side of a golden
rectangle in the case of \code{x} being the longer side and \code{x} being
the shorter side. To get the ratio, use \code{golden(1)}.
}
