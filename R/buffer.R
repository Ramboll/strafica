# -*- coding: us-ascii-unix -*-

#' Create polygon buffers around lines.
#' @param lines a data frame of lines or a list as returned by
#' \code{\link{sp.to.lines}} or a SpatialLinesDataFrame object.
#' @param width amount in coordinate units to offset by.
#' @param byid \code{TRUE} to produce separate objects.
#' @param quadsegs amount of segments to use in a quarter circle.
#' @param ... passed to \code{\link[rgeos]{gBuffer}}.
#' @return Polygons as either a list or
#' a SpatialPolygonsDataFrame object, depending on given input.
#' @rdname buffer.lines
#' @export
buffer.lines = function(lines, width, byid, quadsegs=10, ...)
    UseMethod("buffer.lines", lines)

#' @rdname buffer.lines
#' @method buffer.lines data.frame
#' @export
buffer.lines.data.frame = function(lines, width, byid, quadsegs=10, ...) {
    lines = lines.to.sp(lines)
    buffer = buffer.lines(lines, width, byid, quadsegs, ...)
    return(sp.to.polys(buffer)$outlines)
}

#' @rdname buffer.lines
#' @method buffer.lines list
#' @export
buffer.lines.list = function(lines, width, byid, quadsegs=10, ...) {
    lines = lines.to.sp(lines$lines, lines$data)
    buffer = buffer.lines(lines, width, byid, quadsegs, ...)
    buffer = sp.to.polys(buffer)
    buffer$data = leftjoin(buffer$data, lines$data, by="eid")
    buffer$areas = attach.holes(buffer$outlines)
    return(buffer)
}

#' @rdname buffer.lines
#' @method buffer.lines SpatialLinesDataFrame
#' @export
buffer.lines.SpatialLinesDataFrame = function(lines, width, byid, quadsegs=10, ...) {
    buffer = rgeos::gBuffer(lines, width=width, byid=byid, quadsegs=quadsegs, ...)
    if (!methods::is(buffer, "SpatialPolygonsDataFrame")) {
        # Data will be lacking if byid is FALSE.
        data = data.frame(eid=seq_len(length(buffer@polygons)))
        rownames(data) =  unlist(lapply(buffer@polygons, function(x) x@ID))
        buffer = sp::SpatialPolygonsDataFrame(buffer, data)
    }
    return(buffer)
}

#' Create polygon buffers around points.
#' @param points a data frame of points or
#' a SpatialPointsDataFrame object.
#' @param width amount in coordinate units to offset by.
#' @param byid \code{TRUE} to produce separate objects.
#' @param quadsegs amount of segments to use in a quarter circle.
#' @param ... passed to \code{\link[rgeos]{gBuffer}}.
#' @return Polygons as either a list or
#' a SpatialPolygonsDataFrame object, depending on given input.
#' @rdname buffer.points
#' @export
buffer.points = function(points, width, byid, quadsegs=10, ...)
    UseMethod("buffer.points", points)

#' @rdname buffer.points
#' @method buffer.points data.frame
#' @export
buffer.points.data.frame = function(points, width, byid, quadsegs=10, ...) {
    points = points.to.sp(points)
    buffer = buffer.points(points, width, byid, quadsegs, ...)
    return(sp.to.polys(buffer))
}

#' @rdname buffer.points
#' @method buffer.points SpatialPointsDataFrame
#' @export
buffer.points.SpatialPointsDataFrame = function(points, width, byid, quadsegs=10, ...) {
    buffer = rgeos::gBuffer(points, width=width, byid=byid, quadsegs=quadsegs, ...)
    if (!methods::is(buffer, "SpatialPolygonsDataFrame")) {
        # Data will be lacking if byid is FALSE.
        data = data.frame(eid=seq_len(length(buffer@polygons)))
        rownames(data) =  unlist(lapply(buffer@polygons, function(x) x@ID))
        buffer = sp::SpatialPolygonsDataFrame(buffer, data)
    }
    return(buffer)
}

#' Create polygon buffers around polygons.
#' @param polys a data frame of polygon outlines or a list as returned by
#' \code{\link{sp.to.polys}} or a SpatialPolygonsDataFrame object.
#' @param width amount in coordinate units to offset by.
#' @param byid \code{TRUE} to produce separate objects.
#' @param quadsegs amount of segments to use in a quarter circle.
#' @param ... passed to \code{\link[rgeos]{gBuffer}}.
#' @return Polygons as either a list or
#' a SpatialPolygonsDataFrame object, depending on given input.
#' @rdname buffer.polys
#' @export
buffer.polys = function(polys, width, byid, quadsegs=10, ...)
    UseMethod("buffer.polys", polys)

#' @rdname buffer.polys
#' @method buffer.polys data.frame
#' @export
buffer.polys.data.frame = function(polys, width, byid, quadsegs=10, ...) {
    polys = polys.to.sp(polys)
    buffer = buffer.polys(polys, width, byid, quadsegs, ...)
    return(sp.to.polys(buffer)$outlines)
}

#' @rdname buffer.polys
#' @method buffer.polys list
#' @export
buffer.polys.list = function(polys, width, byid, quadsegs=10, ...) {
    polys = polys.to.sp(polys$outlines, polys$data)
    buffer = buffer.polys(polys, width, byid, quadsegs, ...)
    buffer = sp.to.polys(buffer)
    buffer$data = leftjoin(buffer$data, polys$data, by="eid")
    buffer$areas = attach.holes(buffer$outlines)
    return(buffer)
}

#' @rdname buffer.polys
#' @method buffer.polys SpatialPolygonsDataFrame
#' @export
buffer.polys.SpatialPolygonsDataFrame = function(polys, width, byid, quadsegs=10, ...) {
    buffer = rgeos::gBuffer(polys, width=width, byid=byid, quadsegs=quadsegs, ...)
    if (!methods::is(buffer, "SpatialPolygonsDataFrame")) {
        # Data will be lacking if byid is FALSE.
        data = data.frame(eid=seq_len(length(buffer@polygons)))
        rownames(data) =  unlist(lapply(buffer@polygons, function(x) x@ID))
        buffer = sp::SpatialPolygonsDataFrame(buffer, data)
    }
    return(buffer)
}
