% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{subset.data.frame}
\alias{subset.data.frame}
\title{Subset data frame.}
\usage{
\method{subset}{data.frame}(x, subset, select, drop = FALSE, ...)
}
\arguments{
\item{x}{data frame to be subsetted.}

\item{subset}{logical expression indicating rows to keep:
missing values are taken as false.}

\item{select}{expression, indicating columns to select.}

\item{drop}{passed on to \code{[} indexing operator.}

\item{...}{not used.}
}
\value{
A data frame.
}
\description{
Subset data frame.
}
