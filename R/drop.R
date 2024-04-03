# -*- coding: us-ascii-unix -*-

#' Drop points outside bounding polygon.
#' @param points a data frame of points or a SpatialPointsDataFrame object.
#' @param ... bounds either as a data frame of polygon outlines,
#' a SpatialPolygonsDataFrame object or bounding box coordinates
#' \code{(xmin=..., xmax=..., ymin=..., ymax=...)}.
#' @return Points in same format as \code{points}.
#' @rdname drop.points
#' @export
drop.points = function(points, ...)
    UseMethod("drop.points", points)

#' @rdname drop.points
#' @method drop.points data.frame
#' @export
drop.points.data.frame = function(points, ...) {
    dots = list(...)
    if (length(dots) == 4) {
        # Bounding box coordinates (for legacy compatibility).
        names(dots) = c("xmin", "xmax", "ymin", "ymax")
        points = subset(points, x >= dots$xmin & x <= dots$xmax)
        points = subset(points, y >= dots$ymin & y <= dots$ymax)
        return(points)
    }
    points = points.to.sp(points)
    points = drop.points(points, ...)
    return(sp.to.points(points))
}

#' @rdname drop.points
#' @method drop.points SpatialPointsDataFrame
#' @export
drop.points.SpatialPointsDataFrame = function(points, ...) {
    stop(
        "The function 'drop.points' for SpatialLinesDataFrame objects has",
        "been removed from this package."
    )
}

#' Drop shapes outside bounding box.
#' @param shapes a data frame of shapes.
#' @param xmin west edge of the bounding box.
#' @param xmax east edge of the bounding box.
#' @param ymin south edge of the bounding box.
#' @param ymax north edge of the bounding box.
#' @return A data frame of shapes.
#' @export drop.shapes
drop.shapes = function(shapes, xmin, xmax, ymin, ymax) {
    if ("eid" %in% colnames(shapes) &&
        "sid" %in% colnames(shapes)) {
        id = classify(shapes$eid, shapes$sid)
    } else if ("eid" %in% colnames(shapes)) {
        id = shapes$eid
    } else {
        id = rows.along(shapes)
    }
    inside = with(shapes, x >= xmin & x <= xmax & y >= ymin & y <= ymax)
    inside[is.na(inside)] = FALSE
    return(shapes[which(id %in% id[inside]),,drop=FALSE])
}
