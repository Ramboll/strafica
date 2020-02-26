# -*- coding: us-ascii-unix -*-

#' Draw a random sample.
#'
#' Act as a better substitute for \code{\link{sample}}, which behaves
#' in a silly manner when the length of \code{x} is one.
#' @param x a vector of elements from which to choose.
#' @param ... passed to \code{\link{sample}}.
#' @return A vector of random elements.
#' @export resample
resample = function(x, ...)
    x[sample.int(length(x), ...)]

#' Round to multiple of any number.
#' @param x a numeric vector.
#' @param accuracy number to round to.
#' @param fun rounding function: \code{\link{floor}}, \code{\link{ceiling}}
#' or \code{\link{round}}.
#' @return A numeric or integer vector.
#' @export round_any
round_any = function(x, accuracy, fun=round)
    # Copied and adapted from plyr::round_any,
    # http://github.com/hadley/plyr
    # Copyright Hadley Wickham, MIT-licensed.
    fun(x/accuracy) * accuracy

#' Round coordinate to grid cell center point.
#' @param x a numeric object.
#' @param cell.size size of cells in grid.
#' @return A numeric or integer object.
#' @method round.grid not.an.s3.method
#' @export round.grid
round.grid = function(x, cell.size) {
    x = round_any(x, cell.size, floor) + cell.size/2
    if (all(x %% 1 == 0) && all(x < .Machine$integer.max))
        # Convert metric coordinates to integers.
        x = as.integer(x)
    return(x)
}

#' Round stochastically with floor and ceiling.
#' @param x a numeric object.
#' @return An integer object.
#' @method round.stoc not.an.s3.method
#' @export round.stoc
round.stoc = function(x) {
    m = which(is.finite(x))
    down = stats::rbinom(length(m), 1, x[m] %% 1) == 0
    x[m] = ifelse(down, floor(x[m]), ceiling(x[m]))
    return(as.integer(x))
}

#' Round stochastically with floor and ceiling, keeping total intact.
#' @param x a numeric object.
#' @return An integer object.
#' @method round.total not.an.s3.method
#' @export round.total
round.total = function(x) {
    .sum = function(x)
        round(sum(as.numeric(x), na.rm=TRUE))
    total = .sum(x)
    y = round.stoc(x)
    dev = .sum(y) - total
    # Sample elements to be flipped from among
    # those that contribute to the deviation.
    m = which(sign(y - x) == sign(dev))
    m = resample(m, abs(dev))
    y[m] = if (dev > 0) floor(x[m]) else ceiling(x[m])
    return(as.integer(y))
}

#' Statistical mode.
#' @param x a numeric object.
#' @param na.rm \code{TRUE} to ignore \code{NA} elements.
#' @return A numeric vector of length one.
#' @export stat.mode
stat.mode = function(x, na.rm=FALSE) {
    if (na.rm)
        x = non.na(x)
    ux = unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
}

#' Weighted interquartile range.
#' @param x a numeric object.
#' @param weights a numeric object of non-negative values.
#' @param na.rm \code{TRUE} to ignore \code{NA} elements.
#' @return A numeric vector of length one.
#' @export weighted.iqr
weighted.iqr = function(x, weights, na.rm=FALSE) {
    iqr = diff(weighted.quantile(as.numeric(x),
                                 weights,
                                 probs=c(0.25, 0.75),
                                 na.rm=na.rm))

    names(iqr) = NULL
    return(iqr)
}

#' Weighted median.
#' @param x a numeric object.
#' @param weights a numeric object of non-negative values.
#' @param na.rm \code{TRUE} to ignore \code{NA} elements.
#' @return A numeric vector of length one.
#' @export weighted.median
weighted.median = function(x, weights, na.rm=FALSE) {
    med = weighted.quantile(as.numeric(x),
                            weights,
                            probs=0.5,
                            na.rm=na.rm)

    names(med) = NULL
    return(med)
}

#' Weighted statistical mode.
#' @param x a numeric object.
#' @param weights a numeric object of non-negative values.
#' @param na.rm \code{TRUE} to ignore \code{NA} elements.
#' @return A numeric vector of length one.
#' @export weighted.mode
weighted.mode = function(x, weights, na.rm=FALSE) {
    stat = data.frame(x=x, weight=weights)
    if (na.rm)
        stat = subset(stat, !is.na(x))
    if (nrow(stat) == 0) return(NA)
    if (all(is.na(stat$x))) return(NA)
    if (any(duplicated(stat$x)))
        stat = fold(stat, .(x), weight=sum(weight))
    return(stat$x[which.max(stat$weight)])
}

#' Weighted quantiles.
#'
#' \code{weighted.quantile} does piecewise linear interpolation between
#' midpoints of steps of the empirical cdf. This matches the behaviour of
#' \code{\link{quantile}} given argument \code{type=5}.
#' @param x a numeric object.
#' @param weights a numeric object of non-negative values.
#' @param probs a numeric vector of probabilities.
#' @param na.rm \code{TRUE} to ignore \code{NA} elements.
#' @return A numeric vector same length as \code{probs}.
#' @export weighted.quantile
weighted.quantile = function(x, weights, probs=seq(0, 1, 1/4), na.rm=FALSE) {
    stat = data.frame(x=x, weight=weights)
    if (na.rm)
        stat = subset(stat, !is.na(x))
    if (nrow(stat) == 0) return(NA)
    if (all(is.na(stat$x))) return(NA)
    stat = arrange(stat, x)
    stat$weight = stat$weight / sum(stat$weight)
    # Calculate the midpoints along the steps of the empirical cdf.
    a = c(0, cumsum(stat$weight)[-nrow(stat)])
    z = cumsum(stat$weight)
    cdf = rowMeans(cbind(a, z))
    breaks = sapply(probs, function(prob) {
        if (prob == 0) return(min(stat$x))
        if (prob == 1) return(max(stat$x))
        if (any(cdf == prob))
            return(mean(stat$x[cdf == prob]))
        k1 = max(which(cdf < prob), 1)
        k2 = min(which(cdf > prob), nrow(stat))
        w1 = (cdf[k2] - cdf[k1]) - abs(prob - cdf[k1])
        w2 = (cdf[k2] - cdf[k1]) - abs(prob - cdf[k2])
        return(stats::weighted.mean(stat$x[c(k1, k2)], c(w1, w2)))
    })
    names(breaks) = sprintf("%.0f%%", probs * 100)
    return(breaks)
}
