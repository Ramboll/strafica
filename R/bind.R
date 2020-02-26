# -*- coding: us-ascii-unix -*-

#' Combine sets of lines to form a single collection.
#' @param ... sets of lines as either data frames, lists as
#' returned by \code{\link{sp.to.lines}} or SpatialLinesDataFrames.
#' @return Lines in same format as given.
#' @rdname bind.lines
#' @export
bind.lines = function(...)
    UseMethod("bind.lines")

#' @rdname bind.lines
#' @method bind.lines data.frame
#' @export
bind.lines.data.frame = function(...) {
    sets = list(...)
    if (length(sets) == 0)
        return(NULL)
    out = sets[[1]]
    for (i in seq_along(sets)[-1])
        out = .rbind.eid(out, sets[[i]])
    return(out)
}

#' @rdname bind.lines
#' @method bind.lines list
#' @export
bind.lines.list = function(...) {
    sets = list(...)
    if (length(sets) == 0)
        return(NULL)
    out = sets[[1]]
    for (i in seq_along(sets)[-1]) {
        out$data = .rbind.eid(out$data, sets[[i]]$data)
        out$lines = .rbind.eid(out$lines, sets[[i]]$lines)
    }
    return(out)
}

#' @rdname bind.lines
#' @method bind.lines SpatialLinesDataFrame
#' @export
bind.lines.SpatialLinesDataFrame = function(...) {
    stop("Not implemented")
}

#' Combine sets of points to form a single collection.
#' @param ... sets of points as either data frames or SpatialPointsDataFrames.
#' @return Points in same format as given.
#' @rdname bind.points
#' @export
bind.points = function(...)
    UseMethod("bind.points")

#' @rdname bind.points
#' @method bind.points data.frame
#' @export
bind.points.data.frame = function(...) {
    sets = list(...)
    if (length(sets) == 0)
        return(NULL)
    out = sets[[1]]
    for (i in seq_along(sets)[-1])
        out = .rbind.eid(out, sets[[i]])
    return(out)
}

#' @rdname bind.points
#' @method bind.points SpatialPointsDataFrame
#' @export
bind.points.SpatialPointsDataFrame = function(...) {
    stop("Not implemented")
}

#' Combine sets of polygons to form a single collection.
#' @param ... sets of polygons as either data frames, lists as
#' returned by \code{\link{sp.to.polys}} or SpatialPolygonsDataFrames.
#' @return Polygons in same format as given.
#' @rdname bind.polys
#' @export
bind.polys = function(...)
    UseMethod("bind.polys")

#' @rdname bind.polys
#' @method bind.polys data.frame
#' @export
bind.polys.data.frame = function(...) {
    sets = list(...)
    if (length(sets) == 0)
        return(NULL)
    out = sets[[1]]
    for (i in seq_along(sets)[-1])
        out = .rbind.eid(out, sets[[i]])
    return(out)
}

#' @rdname bind.polys
#' @method bind.polys list
#' @export
bind.polys.list = function(...) {
    sets = list(...)
    if (length(sets) == 0)
        return(NULL)
    out = sets[[1]]
    for (i in seq_along(sets)[-1]) {
        out$data = .rbind.eid(out$data, sets[[i]]$data)
        out$areas = .rbind.eid(out$areas, sets[[i]]$areas)
        out$outlines = .rbind.eid(out$outlines, sets[[i]]$outlines)
    }
    return(out)
}

#' @rdname bind.polys
#' @method bind.polys SpatialPolygonsDataFrame
#' @export
bind.polys.SpatialPolygonsDataFrame = function(...) {
    stop("Not implemented")
}

# Combine data frames, bumping eid values of y.
.rbind.eid = function(x, y) {
    if (is.null(x) && is.null(y))
        return(NULL)
    stopif(xor(is.null(x), is.null(y)))
    y$eid = max(x$eid) + y$eid
    return(rbind_list(x, y))
}
