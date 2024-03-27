# -*- coding: us-ascii-unix -*-

#' Clip lines to a bounding polygon.
#' @param lines a data frame of lines or a list as returned by
#' \code{\link{sp.to.lines}} or a SpatialLinesDataFrame object.
#' @param ... bounds either as a data frame of polygon outlines,
#' a SpatialPolygonsDataFrame object or bounding box coordinates
#' \code{(xmin=..., xmax=..., ymin=..., ymax=...)}.
#' @return Lines in same format as \code{lines}.
#' @rdname clip.lines
#' @export
clip.lines = function(lines, ...)
    UseMethod("clip.lines", lines)

#' @rdname clip.lines
#' @method clip.lines data.frame
#' @export
clip.lines.data.frame = function(lines, ...) {
    dots = list(...)
    if (length(dots) == 4) {
        # Bounding box coordinates (for legacy compatibility).
        names(dots) = c("xmin", "xmax", "ymin", "ymax")
        lines = with(dots, drop.shapes(
            lines, xmin, xmax, ymin, ymax))
    }
    lines = lines.to.sp(lines)
    lines = clip.lines(lines, ...)
    return(sp.to.lines(lines)$lines)
}

#' @rdname clip.lines
#' @method clip.lines list
#' @export
clip.lines.list = function(lines, ...) {
    dots = list(...)
    if (length(dots) == 4) {
        # Bounding box coordinates (for legacy compatibility).
        names(dots) = c("xmin", "xmax", "ymin", "ymax")
        lines$lines = with(dots, drop.shapes(
            lines$lines, xmin, xmax, ymin, ymax))
        lines$data = subset(lines$data, eid %in% lines$lines$eid)
    }
    lines = lines.to.sp(lines$lines, lines$data)
    lines = clip.lines(lines, ...)
    return(sp.to.lines(lines))
}

#' @rdname clip.lines
#' @method clip.lines SpatialLinesDataFrame
#' @export
clip.lines.SpatialLinesDataFrame = function(lines, ...) {
    stop(
        "The function 'clip.lines' for SpatialLinesDataFrame objects has",
        "been removed from this package."
    )
}

#' Clip polygons to a bounding polygon.
#' @param polys a data frame of polygon outlines or a list as returned by
#' \code{\link{sp.to.polys}} or a SpatialPolygonsDataFrame object.
#' @param ... bounds either as a data frame of polygon outlines,
#' a SpatialPolygonsDataFrame object or bounding box coordinates
#' \code{(xmin=..., xmax=..., ymin=..., ymax=...)}.
#' @return Polygons in same format as \code{polys}.
#' @rdname clip.polys
#' @export
clip.polys = function(polys, ...)
    UseMethod("clip.polys", polys)

#' @rdname clip.polys
#' @method clip.polys data.frame
#' @export
clip.polys.data.frame = function(polys, ...) {
    if (length(list(...)) == 4) {
        # Bounding box coordinates (for legacy compatibility).
        # Work around various polygon topology etc. related errors
        # by using a simple pure R implementation.
        return(squeeze.polys(polys, ...))
    }
    polys = polys.to.sp(polys)
    polys = clip.polys(polys, ...)
    return(sp.to.polys(polys)$outlines)
}

#' @rdname clip.polys
#' @method clip.polys list
#' @export
clip.polys.list = function(polys, ...) {
    if (length(list(...)) == 4) {
        # Bounding box coordinates (for legacy compatibility).
        # Work around various polygon topology etc. related errors
        # by using a simple pure R implementation.
        polys$outlines = squeeze.polys(polys$outlines, ...)
        polys$areas = attach.holes(polys$outlines)
        polys$data = subset(polys$data, eid %in% polys$outlines$eid)
        return(polys)
    }
    polys = polys.to.sp(polys$outlines, polys$data)
    polys = clip.polys(polys, ...)
    polys = sp.to.polys(polys)
    polys$areas = attach.holes(polys$outlines)
    return(polys)
}

#' @rdname clip.polys
#' @method clip.polys SpatialPolygonsDataFrame
#' @export
clip.polys.SpatialPolygonsDataFrame = function(polys, ...) {
        stop(
        "The function 'clip.polys' for SpatialLinesDataFrame objects has",
        "been removed from this package."
    )
}

#' Clip segments to a bounding polygon.
#' @param segments a data frame of segments.
#' @param ... bounds either as a data frame of polygon outlines,
#' a SpatialPolygonsDataFrame object or bounding box coordinates
#' \code{(xmin=..., xmax=..., ymin=..., ymax=...)}.
#' @return A list of segments.
#' @export clip.segments
clip.segments = function(segments, ...) {
    dots = list(...)
    if (length(dots) == 4) {
        # Bounding box coordinates (for legacy compatibility).
        names(dots) = c("xmin", "xmax", "ymin", "ymax")
        eid = rows.along(segments)
        ipoints = data.frame(eid=eid, x=segments$ix, y=segments$iy)
        jpoints = data.frame(eid=eid, x=segments$jx, y=segments$jy)
        ipoints = drop.points(ipoints, ...)
        jpoints = drop.points(jpoints, ...)
        ii = sort(union(ipoints$eid, jpoints$eid))
        segments = segments[ii,,drop=FALSE]
    }
    lines = segments.to.sp(segments)
    lines = clip.lines(lines, ...)
    lines = sp.to.lines(lines)
    lines = leftjoin(lines$lines, lines$data, by="eid")
    return(lines.to.segments(lines))
}

#' Squeeze polygons to bounding box.
#' @param polys a data frame of polygons.
#' @param xmin west edge of the bounding box.
#' @param xmax east edge of the bounding box.
#' @param ymin south edge of the bounding box.
#' @param ymax north edge of the bounding box.
#' @return A data frame of polygons.
#' @export squeeze.polys
squeeze.polys = function(polys, xmin, xmax, ymin, ymax) {
    message("Squeezing polygons to bounding box...")
    polys = drop.shapes(polys, xmin, xmax, ymin, ymax)
    polys$x = pclip(polys$x, xmin, xmax)
    polys$y = pclip(polys$y, ymin, ymax)
    # Remove consecutive duplicate points.
    id = with(polys, classify(eid, sid, x, y))
    polys = polys[which(c(TRUE, diff(id) != 0)),,drop=FALSE]
    polys = polysimp.polys(polys, epsilon=0, method=visvalingam)
    return(polys)
}
