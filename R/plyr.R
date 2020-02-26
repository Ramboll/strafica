# -*- coding: us-ascii-unix -*-

#' ddply from plyr.
#' @name __ddply__
#' @rdname ddply
#' @importFrom plyr ddply
NULL

#' Split data frame and calculate group-wise summaries.
#' @param .data data frame to be processed
#' @param .variables variables to split data frame by, as as.quoted variables, a
#'   formula or character vector
#' @param .fun function to apply to each piece
#' @param ... See \code{\link[plyr]{ddply}} in \pkg{plyr}.
#' @param .progress name of the progress bar to use, see
#'   \code{\link[plyr]{create_progress_bar}}.
#' @param .inform produce informative error messages?
#' @param .drop should combinations of variables that do not appear in the input
#'   data be preserved (\code{FALSE}) or dropped (\code{TRUE}, default)
#' @param .parallel not applicable, see \code{\link{mcddply}} instead.
#' @param .paropts not applicable, see \code{\link{mcddply}} instead.
#' @export ddply
ddply = plyr::ddply

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
