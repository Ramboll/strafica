# -*- coding: us-ascii-unix -*-

#' Convert degrees to radians.
#' @param deg numeric object of degrees.
#' @return A numeric object of radians.
#' @export deg2rad
deg2rad = function(deg)
    deg * pi / 180

#' Determine shape ID values from data.
#'
#' Please note that elsewhere shape IDs may be indexed to be unique across one
#' entry, i.e. starting from one for each "eid", but \code{index.shapes} will
#' return shape IDs that are unique across all entries. This is done for
#' performance reasons and should not have any practical implications.
#' @param shapes a data frame of shapes.
#' @return An integer vector of shape IDs.
#' @export index.shapes
index.shapes = function(shapes)
    cumsum(c(FALSE, is.na(shapes$x)[-nrow(shapes)])) + 1L

#' Angles perpendicular to a segments.
#' @param x1 x-coordinates of first points.
#' @param y1 y-coordinates of first points.
#' @param x2 x-coordinates of second points.
#' @param y2 y-coordinates of second points.
#' @return A numeric object of angles in radians.
#' @export perp.angle
perp.angle = function(x1, y1, x2, y2) {
    theta = atan2(y2 - y1, x2 - x1)
    return(-1 * sign(theta) * (pi/2 - abs(theta)))
}

#' Convert radians to degrees.
#' @param rad numeric object of radians.
#' @return A numeric object of degrees.
#' @export rad2deg
rad2deg = function(rad)
    rad * 180 / pi

#' 2d spatially weighted mean.
#'
#' Values are weighted linearly with the weight at zero distance being one and
#' at radius distance being zero. These distance weights are further multiplied
#' by \code{weights} if given.
#' @param points a data frame of points.
#' @param value a numeric vector of values.
#' @param radius radius of circle within which to weigh values.
#' @param weights a numeric vector of weights.
#' @param na.rm \code{TRUE} to ignore \code{NA} elements.
#' @return A numeric vector of weighted values.
#' @export spatial.mean
spatial.mean = function(points, value, radius, weights=1, na.rm=FALSE) {
    points = post.gc(pick(points, x, y))
    if (length(weights) == 1)
        weights = rep(weights, length(value))
    time.start = Sys.time()
    messagef("Calculating spatial mean of %d points...", nrow(points))
    means = mclapply.stop(seq_along(value), function(i) {
        if (progress.due(3, parallel=TRUE))
            progress.eta(time.start, i, length(value))
        m = with(points, which(abs(x - x[i]) < radius & abs(y - y[i]) < radius))
        dist = eucd(points$x[i], points$y[i], points$x[m], points$y[m])
        weights = weights[m] * pmax(0, 1 - dist/radius)
        return(stats::weighted.mean(value[m], weights, na.rm=na.rm))
    })
    means = unlist(means)
    progress.final(time.start)
    return(means)
}
