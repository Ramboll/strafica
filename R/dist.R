# -*- coding: us-ascii-unix -*-

#' Euclidean distance.
#' @param x1 x-coordinates of first points.
#' @param y1 y-coordinates of first points.
#' @param x2 x-coordinates of second points.
#' @param y2 y-coordinates of second points.
#' @return A numeric object.
#' @seealso \code{\link{eucd2}}
#' @export eucd
eucd = function(x1, y1, x2, y2) {
    xd = as.numeric(x1 - x2)
    yd = as.numeric(y1 - y2)
    return(sqrt(xd*xd + yd*yd))
}

#' Squared euclidean distance.
#'
#' Omitting the square root makes this much faster to calculate than \code{eucd}.
#' While this is not the actual euclidean distance, the square is sufficient for
#' some cases, e.g. finding the nearest neighbour.
#' @param x1 x-coordinates of first points.
#' @param y1 y-coordinates of first points.
#' @param x2 x-coordinates of second points.
#' @param y2 y-coordinates of second points.
#' @return A numeric object.
#' @seealso \code{\link{eucd}}
#' @export eucd2
eucd2 = function(x1, y1, x2, y2) {
    xd = as.numeric(x1 - x2)
    yd = as.numeric(y1 - y2)
    return(xd*xd + yd*yd)
}

#' Distances of points from lines.
#'
#' Either \code{p} or \code{a} and \code{b} can be vectorized, not both.
#' Coordinates should be given as either a numeric vector of length two
#' or a two-column matrix of X- and Y-coordinates.
#' @param p coordinates of points.
#' @param a coordinates of start points of lines.
#' @param b coordinates of end points of lines.
#' @return A numeric vector of distances.
#' @references \url{http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html}
#' @export point.line.dist
point.line.dist = function(p, a, b) {
    if (is.matrix(p) && (is.matrix(a) || is.matrix(b)))
        stop("Either points or lines can be matrices, not both")
    # Use numeric to avoid integer overflow.
    px = as.numeric(if (is.matrix(p)) p[,1] else p[1])
    py = as.numeric(if (is.matrix(p)) p[,2] else p[2])
    ax = as.numeric(if (is.matrix(a)) a[,1] else a[1])
    ay = as.numeric(if (is.matrix(a)) a[,2] else a[2])
    bx = as.numeric(if (is.matrix(b)) b[,1] else b[1])
    by = as.numeric(if (is.matrix(b)) b[,2] else b[2])
    dist = abs((bx - ax) * (ay - py) - (ax - px) * (by - ay)) / eucd(ax, ay, bx, by)
    # Calculate distance from point for zero-length lines.
    zero = is.finite(px) & ax == bx & ay == by
    return(ifelse(zero, eucd(px, py, ax, ay), dist))
}

#' Distances of points from segments.
#'
#' Either \code{p} or \code{a} and \code{b} can be vectorized, not both.
#' Coordinates should be given as either a numeric vector of length two
#' or a two-column matrix of X- and Y-coordinates.
#' @param p coordinates of points.
#' @param a coordinates of the start points of segments.
#' @param b coordinates of the end points of segments.
#' @return A numeric vector of distances.
#' @references \url{http://paulbourke.net/geometry/pointlineplane/}
#' @export point.segment.dist
point.segment.dist = function(p, a, b) {
    if (is.matrix(p) && (is.matrix(a) || is.matrix(b)))
        stop("Either points or segments can be matrices, not both")
    # Use numeric to avoid integer overflow.
    px = as.numeric(if (is.matrix(p)) p[,1] else p[1])
    py = as.numeric(if (is.matrix(p)) p[,2] else p[2])
    ax = as.numeric(if (is.matrix(a)) a[,1] else a[1])
    ay = as.numeric(if (is.matrix(a)) a[,2] else a[2])
    bx = as.numeric(if (is.matrix(b)) b[,1] else b[1])
    by = as.numeric(if (is.matrix(b)) b[,2] else b[2])
    u = (((px - ax) * (bx - ax) + (py - ay) * (by - ay)) / eucd2(ax, ay, bx, by))
    x = ax + u * (bx - ax)
    y = ay + u * (by - ay)
    dist = eucd(px, py, x, y)
    # Calculate distance to nearest endpoint if either
    # outside the segment or if segment of zero-length.
    outside = !is.na(u) & (u < 0 | u > 1)
    zero = is.finite(px) & ax == bx & ay == by
    odist = pmin(eucd(px, py, ax, ay), eucd(px, py, bx, by))
    return(ifelse(outside | zero, odist, dist))
}
