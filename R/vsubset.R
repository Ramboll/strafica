# -*- coding: us-ascii-unix -*-

#' Return subset of data frames which meet conditions and reported number of rows dropped.
#' Wrapper function around \code{\link[base]{subset}}.
#' @param df data frame to be subsetted.
#' @param ... passed on to \code{\link[base]{subset}}.
#' @return A data frame.
#' @export vsubset
vsubset = function(df, ...) {
    n1 = nrow(df)
    df = subset(df, ...)
    n2 = nrow(df)
    rows = abs(n2-n1)
    message(sprintf("rows deleted: %s", rows))
    return(df)
}