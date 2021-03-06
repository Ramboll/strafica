% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{write.xtabs}
\alias{write.xtabs}
\title{Write xtabs table to file.}
\usage{
write.xtabs(x, fname, prefix = "", quote = FALSE, sep = "\\t", ...)
}
\arguments{
\item{x}{The object to be written, preferably a matrix or data frame.}

\item{fname}{A character string naming a file.}

\item{prefix}{A dumb variable to account for the first missing cell of a
contingency table.}

\item{quote}{A logical value (\code{TRUE} or \code{FALSE}) or a numeric 
vector. If \code{TRUE}, any character or factor columns will be surrounded 
by double quotes. If a numeric vector, its elements are taken as the
indices of columns to quote. In both cases, row and column names are quoted
if they are written. If \code{FALSE}, nothing is quoted.}

\item{sep}{The field separator string.}

\item{...}{Further arguments to be passed to
\code{\link[utils]{write.table}}.}
}
\description{
See \code{\link{write.table}} for arguments.
}
