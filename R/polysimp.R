# -*- coding: us-ascii-unix -*-

#' Simplify polyline with the Douglas-Peucker algorithm.
#' @param coords a two-column matrix of coordinates of polyline nodes.
#' @param epsilon threshold, below which nodes will be removed.
#' @param static a logical vector indicating nodes which should not be removed.
#' @param max.length maximum length of line segments to form.
#' @return A logical vector of rows to keep.
#' @export dp
dp = function(coords, epsilon, static=NULL, max.length=Inf) {
    n = nrow(coords)
    if (n < 3) return(rep(TRUE, nrow(coords)))
    if (is.null(static))
        static = rep(FALSE, n)
    # Keep track of unhandled candidates for simplified segment
    # start and end points formed by connecting accepted nodes.
    start = c(TRUE, rep(FALSE, nrow(coords) - 1))
    end = c(rep(FALSE, nrow(coords) - 1), TRUE)
    keep = start | end
    while (any(start)) {
        a = match(TRUE, start)
        z = match(TRUE, end)
        stopifnot(z > a + 1)
        dist = point.segment.dist(coords[(a+1):(z-1),], coords[a,], coords[z,])
        dist[static[(a+1):(z-1)]] = Inf
        if (is.finite(max.length)) {
            post.length = sum(eucd(coords[a,1],
                                   coords[a,2],
                                   coords[z,1],
                                   coords[z,2]))

        } else {
            post.length = 0
        }
        start[a] = end[z] = FALSE
        if (max(dist) > epsilon || post.length > max.length) {
            # In case of static points, many can have infinite distance.
            # We might be able to avoid a worst-case scenario of one item
            # at a time iteration by using a point from the middle.
            if (any(is.infinite(dist))) {
                b = which(is.infinite(dist))
                b = b[ceiling(length(b)/2)]
            } else {
                b = which.max(dist)
            }
            i = a + b
            if (i > a + 1) start[a] = end[i] = TRUE
            if (z > i + 1) start[i] = end[z] = TRUE
            keep[i] = TRUE
        }
    }
    return(keep)
}

#' Simplify polyline.
#' @param coords a two-column matrix of coordinates of polyline nodes.
#' @param epsilon threshold, below which nodes will be removed.
#' @param static a logical vector indicating nodes which should not be removed.
#' @param max.length maximum length of line segments to form.
#' @return A logical vector of rows to keep.
#' @export polysimp
polysimp = function(coords, epsilon, static=NULL, max.length=Inf)
    dp(coords, epsilon, static, max.length)

#' Simplify lines by dropping visually least significant points.
#'
#' All duplicated nodes are kept intact.
#' @param lines a data frame of lines.
#' @param epsilon threshold, below which nodes will be removed.
#' @param static a logical vector indicating nodes which should not be removed.
#' @param max.length maximum length of line segments to form.
#' @param keep.double keep all duplicated nodes intact. This is necessary
#' to keep noded intersections in place, e.g. with a road network to maintain
#' connectivity for routing use.
#' @param method low-level polygon simplification function to use:
#' \code{\link{dp}} for Douglas-Peucker's algorithm or
#' \code{\link{visvalingam}} for Visvalingam's algorithm.
#' @return A data frame of lines.
#' @export polysimp.lines
polysimp.lines = function(lines, epsilon, static=NULL, max.length=Inf,
                          keep.double=TRUE, method=dp) {

    id = classify(lines$eid, lines$sid)
    first = which(c(Inf, diff(id)) != 0)
    last = c(first[-1] - 1, length(id))
    if (is.null(static))
        static = rep(FALSE, nrow(lines))
    static[first] = TRUE
    static[(last-1)] = TRUE
    if (keep.double) {
        xy = classify(lines$x, lines$y)
        static = static | xy %in% xy[duplicated(xy)]
        post.gc(rm(xy))
    }
    time.start = Sys.time()
    message("Simplifying lines...")
    keep = mclapply.stop(seq_along(first), function(i) {
        if (progress.due(3, parallel=TRUE))
            progress.eta(time.start, i, length(first))
        if (last[i] <= first[i])
            return(NULL)
        m = first[i]:(last[i]-1)
        keep = method(cbind(lines$x[m], lines$y[m]),
                      epsilon,
                      static[m],
                      max.length)

        keep = c(keep, any(keep))
        return(c(m, last[i])[keep])
    })
    keep = sort(unlist(keep))
    progress.final(time.start)
    return(lines[keep,,drop=FALSE])
}

#' Simplify polygons by dropping visually least significant points.
#'
#' Simplification is done in two passes. All nodes kept by any polygon
#' in the first pass are kept intact.
#' @param polys a data frame of polygons.
#' @param epsilon threshold, below which nodes will be removed.
#' @param static a logical vector indicating nodes which should not be removed.
#' @param max.length maximum length of line segments to form.
#' @param keep.triple keep all triplicated nodes intact. For adjacent polygons
#' (e.g. administrative boundaries) this is necessary (in addition to the
#' two-pass process) to keep the polygons adjacent.
#' @param method low-level polygon simplification function to use:
#' \code{\link{dp}} for Douglas-Peucker's algorithm or
#' \code{\link{visvalingam}} for Visvalingam's algorithm.
#' @param pass Only true wizards should even consider setting this argument.
#' @return A data frame of polygons.
#' @export polysimp.polys
polysimp.polys = function(polys, epsilon, static=NULL, max.length=Inf,
                          keep.triple=TRUE, method=dp, pass=1) {

    id = classify(polys$eid, polys$sid)
    first = which(c(Inf, diff(id)) != 0)
    last = c(first[-1] - 1, length(id))
    if (is.null(static))
        static = rep(FALSE, nrow(polys))
    static[first] = TRUE
    static[(last-1)] = TRUE
    if (keep.triple) {
        xy1 = classify(polys$x, polys$y)
        xy2 = xy1[duplicated(xy1)]
        xy3 = xy2[duplicated(xy2)]
        static = static | xy1 %in% xy3
        post.gc(rm(xy1, xy2, xy3))
    }
    time.start = Sys.time()
    messagef("Simplifying polygons, pass %d/2...", pass)
    keep = mclapply.stop(seq_along(first), function(i) {
        if (progress.due(3, parallel=TRUE))
            progress.eta(time.start, i, length(first))
        if (last[i] <= first[i])
            return(NULL)
        m = first[i]:(last[i]-1)
        keep = method(cbind(polys$x[m], polys$y[m]),
                      epsilon,
                      static[m],
                      max.length)

        keep = c(keep, any(keep))
        return(c(m, last[i])[keep])
    })
    keep = sort(unlist(keep))
    progress.final(time.start)
    if (pass == 1)
        return(Recall(polys,
                      epsilon=epsilon,
                      static=static|(rows.along(polys) %in% keep),
                      max.length=max.length,
                      keep.triple=keep.triple,
                      method=method,
                      pass=2))

    return(polys[keep,,drop=FALSE])
}

#' Simplify polyline with the Visvalingam algorithm.
#' @param coords a two-column matrix of coordinates of polyline nodes.
#' @param epsilon threshold, below which nodes will be removed. While
#' Visvalingam's algorithm works on areas, \code{epsilon} here should be a
#' length, of similar magnitude as one would use with Douglas-Peucker.
#' @param static a logical vector indicating nodes which should not be removed.
#' @param max.length maximum length of line segments to form.
#' @references \url{http://hydra.hull.ac.uk/resources/hull:8338},
#' \url{http://bost.ocks.org/mike/simplify/}
#' @return A logical vector of rows to keep.
#' @export visvalingam
visvalingam = function(coords, epsilon, static=NULL, max.length=Inf) {
    triangle.area = function(x1, y1, x2, y2, x3, y3)
        (0.5 * eucd(x1, y1, x2, y2) *
         point.line.dist(c(x3, y3), c(x1, y1), c(x2, y2)))
    n = nrow(coords)
    if (n < 3) return(rep(TRUE, nrow(coords)))
    if (is.null(static))
        static = rep(FALSE, n)
    keep = rep(TRUE, n)
    x = coords[,1]
    y = coords[,2]
    # Calculate areas of triangles associated with each point
    # as well as length of segment formed of point were removed.
    area = rep(Inf, n)
    area[2:(n-1)] = sapply(2:(n-1), function(i) {
        triangle.area(x[i-1], y[i-1], x[i], y[i], x[i+1], y[i+1])
    })
    dist = rep(Inf, n)
    dist[2:(n-1)] = sapply(2:(n-1), function(i) {
        eucd(x[i-1], y[i-1], x[i+1], y[i+1])
    })
    area.max = 4 * epsilon * epsilon / 2
    while (TRUE) {
        area[static] = Inf
        area[dist > max.length] = Inf
        i = which.min(area)
        if (area[i] > area.max) break
        keep[i] = FALSE
        area[i] = NA
        # Calculate which points of those left form remaining adjacent triangles
        # and update areas and segment lengths of those triangles. Mark adjacent
        # triangles as g-h-j and h-j-k with the removed i being between h and j.
        left = which(keep)
        h = max(left[left < i])
        j = min(left[left > i])
        g = max(left[left < h], -1)
        k = min(left[left > j], n+1)
        if (g > 0) {
            area[h] = triangle.area(x[g], y[g], x[h], y[h], x[j], y[j])
            dist[h] = eucd(x[g], y[g], x[j], y[j])
        }
        if (k <= n) {
            area[j] = triangle.area(x[h], y[h], x[j], y[j], x[k], y[k])
            dist[j] = eucd(x[h], y[h], x[k], y[k])
        }
    }
    return(keep)
}
