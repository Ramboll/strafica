# -*- coding: us-ascii-unix -*-

#' Find outliers based on inter-quartile range.
#' @param x a numeric vector.
#' @param lower \code{TRUE} to check lower end for outliers.
#' @param upper \code{TRUE} to check upper end for outliers.
#' @param coeff maximum IQR  allowed for non-outlier elements.
#' @param na.rm \code{TRUE} to ignore \code{NA} elements.
#' @return A logical vector indicating which of \code{x} are outliers.
#' @export outliers.iqr
outliers.iqr = function(x, lower=TRUE, upper=TRUE, coeff=1.5, na.rm=TRUE) {
    if (stats::IQR(x, na.rm=na.rm) %in% 0)
        # IQR zero, cannot find outliers.
        return(rep(FALSE, length(x)))
    x.offset = coeff * stats::IQR(x, na.rm=na.rm)
    x.lower = stats::quantile(x, 0.25, na.rm=na.rm) - x.offset
    x.upper = stats::quantile(x, 0.75, na.rm=na.rm) + x.offset
    outliers = rep(FALSE, length(x))
    if (lower) outliers = outliers | (x < x.lower)
    if (upper) outliers = outliers | (x > x.upper)
    outliers[is.na(x)] = FALSE
    return(outliers)
}

#' Find outliers based on median absolute deviation.
#' @param x a numeric vector.
#' @param lower \code{TRUE} to check lower end for outliers.
#' @param upper \code{TRUE} to check upper end for outliers.
#' @param coeff maximum MAD allowed for non-outlier elements.
#' @param na.rm \code{TRUE} to ignore \code{NA} elements.
#' @return A logical vector indicating which of \code{x} are outliers.
#' @export outliers.mad
outliers.mad = function(x, lower=TRUE, upper=TRUE, coeff=3.5, na.rm=TRUE) {
    if (stats::mad(x, na.rm=na.rm) %in% 0)
        # MAD zero, cannot find outliers.
        return(rep(FALSE, length(x)))
    x.median = stats::median(x, na.rm=na.rm)
    x.offset = coeff * stats::mad(x, na.rm=na.rm)
    x.lower = x.median - x.offset
    x.upper = x.median + x.offset
    outliers = rep(FALSE, length(x))
    if (lower) outliers = outliers | (x < x.lower)
    if (upper) outliers = outliers | (x > x.upper)
    outliers[is.na(x)] = FALSE
    return(outliers)
}
