# -*- coding: us-ascii-unix -*-

#' Test against inclusion, opposite of \code{\%in\%}.
#' @param x vector or \code{NULL}: the values to be matched
#' @param table vector or \code{NULL}: the values to be matched against
#' @return A logical vector.
#' @export
#' @name %nin%
#' @rdname nin
#' @usage x \%nin\% table
`%nin%` = function(x, table) {
    return(!(x %in% table))
}
