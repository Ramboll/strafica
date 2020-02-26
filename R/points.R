# -*- coding: us-ascii-unix -*-

#' Find polygons in which points are located.
#'
#' For the usual case of merging data from containing polygons,
#' \code{\link{points.in.polys}} should be more convenient.
#' \code{\link{find.containers}} is better if you need the full geometry,
#' i.e. both eid and sid values of outer polygons.
#' @param x a numeric vector of X-coordinates of points.
#' @param y a numeric vector of Y-coordinates of points.
#' @param outlines a data frame of polygon outlines.
#' @return A data frame of "eid" and "sid" values for each point.
#' For points not inside any polygon, the value will be \code{NA}.
#' @seealso \code{\link{points.in.polys}}
#' @export find.containers
find.containers = function(x, y, outlines) {
    points = data.frame(eid=seq_along(x), x=x, y=y)
    points = points.to.sp(points)
    outlines$eid_real = outlines$eid
    outlines$sid_real = outlines$sid
    outlines$eid = classify(outlines$eid, outlines$sid)
    outlines$sid = 1L
    data = subset(outlines, !duplicated(eid))
    m = points.in.polys(x, y, outlines, data)
    return(data.frame(eid=data$eid_real[m], sid=data$sid_real[m]))
}

#' Find polygons in which points are located.
#' @param x a numeric vector of X-coordinates of points.
#' @param y a numeric vector of Y-coordinates of points.
#' @param outlines a data frame of polygon outlines.
#' @param data a data frame of polygon data associated with outlines.
#' @return If \code{data} given, an integer value vector of indices of
#' points in \code{data}. For points not in any polygon, value will be
#' \code{NA}. If data not given, a logical vector.
#' @seealso \code{\link{find.containers}}
#' @export points.in.polys
#' @method points.in.polys not.an.s3.method
points.in.polys = function(x, y, outlines, data=NULL) {
    data.given = is.data.frame(data)
    if (!data.given)
        data = data.frame(eid=sort(unique(outlines$eid)))
    data = pick(data, eid)
    data$row = rows.along(data)
    polys = polys.to.sp(outlines, data)
    chunks = uind(round_any(seq_along(x), 1000, floor))
    time.start = Sys.time()
    messagef("Finding containing polygons for %d points...", length(x))
    output = mclapply.stop(chunks, function(ii) {
        if (progress.due(3, parallel=TRUE))
            progress.eta(time.start, min(ii), length(x))
        datarow = rep(NA, length(ii))
        m = which(is.finite(x[ii]) & is.finite(y[ii]))
        if (length(m) > 0) {
            points = sp::SpatialPoints(cbind(x[ii[m]], y[ii[m]]))
            output = sp::over(points, polys)
            datarow[m] = output$row
        }
        if (data.given)
            return(datarow)
        return(!is.na(datarow))
    })
    output = unlist(output)
    progress.final(time.start)
    return(output)
}
