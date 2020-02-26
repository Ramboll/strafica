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
    dots = list(...)
    if (length(dots) == 1 && is.data.frame(dots[[1]])) {
        # Polygon outline data frame.
        to = polys.to.sp(dots[[1]])
    } else if (length(dots) == 1 && methods::is(dots[[1]], "SpatialPolygonsDataFrame")) {
        # SpatialPolygonsDataFrame object.
        to = dots[[1]]
    } else if (length(dots) == 4) {
        # Bounding box coordinates (for legacy compatibility).
        names(dots) = c("xmin", "xmax", "ymin", "ymax")
        to = bbox.to.sp(dots)
    } else {
        stop("Bad arguments for bounds")
    }
    message("Dropping points outside bounding polygon...")
    sp::proj4string(points) = sp::proj4string(to) = ""
    keep = which(rgeos::gContains(to, points, byid=TRUE))
    return(points[keep,,drop=FALSE])
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
