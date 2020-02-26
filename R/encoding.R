# -*- coding: us-ascii-unix -*-

#' Convert character encoding.
#' @param x a character vector or a data frame.
#' @param from character encoding to convert from.
#' Use "default" as shorthand for \code{getOption("encoding")}
#' Can be \code{NULL} to try \code{\link{Encoding}}
#' (in case that fails, no conversion is done).
#' @param to character encoding to convert to.
#' Use "default" as shorthand for \code{getOption("encoding")}
#' @param ... not in use
#' @return A object of same type as given.
#' @rdname recode
#' @export
recode = function(x, from=NULL, to, ...)
    UseMethod("recode", x)

#' @rdname recode
#' @method recode character
#' @export
recode.character = function(x, from=NULL, to, ...) {
    if (!is.null(from) && from == "default")
        from = getOption("encoding")
    if (!is.null(to) && to == "default")
        to = getOption("encoding")
    if (is.null(from))
        from = unique(setdiff(Encoding(x), "unknown"))
    if (length(from) != 1) return(x)
    return(iconv(x, from=from, to=to, mark=TRUE))
}

#' @rdname recode
#' @method recode data.frame
#' @export
recode.data.frame = function(x, from=NULL, to, ...) {
    if (!is.null(from) && from == "default")
        from = getOption("encoding")
    if (!is.null(to) && to == "default")
        to = getOption("encoding")
    from.given = from
    for (i in which(sapply(x, is.character))) {
        from = from.given
        if (is.null(from))
            from = unique(setdiff(Encoding(x[,i]), "unknown"))
        if (length(from) != 1) next
        x[,i] = iconv(x[,i], from=from, to=to, mark=TRUE)
    }
    return(x)
}
