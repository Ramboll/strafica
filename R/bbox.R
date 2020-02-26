# -*- coding: us-ascii-unix -*-

#' Convert bounding box coordinates to a polygon data frame.
#' @param bbox a list with items "xmin", "xmax", "ymin" and "ymax".
#' @return A polygon data frame.
#' @export bbox.to.poly
bbox.to.poly = function(bbox)
    with(bbox, data.frame(eid=1L,
                          sid=1L,
                          x=c(xmin, xmin, xmax, xmax, xmin),
                          y=c(ymin, ymax, ymax, ymin, ymin)))

#' Coordinates of a bounding box.
#' @param shapes a data frame of shapes or an \pkg{sp} object.
#' @return A list with items "xmin", "xmax", "ymin" and "ymax".
#' @rdname bounding.box
#' @export
bounding.box = function(shapes)
    UseMethod("bounding.box", shapes)

#' @rdname bounding.box
#' @method bounding.box data.frame
#' @export
bounding.box.data.frame = function(shapes)
    list(xmin=min(shapes$x, na.rm=TRUE),
         xmax=max(shapes$x, na.rm=TRUE),
         ymin=min(shapes$y, na.rm=TRUE),
         ymax=max(shapes$y, na.rm=TRUE))

bounding.box.sp = function(shapes) {
    bounds = sp::bbox(shapes)
    return(list(xmin=bounds["x", "min"],
                xmax=bounds["x", "max"],
                ymin=bounds["y", "min"],
                ymax=bounds["y", "max"]))

}

#' @rdname bounding.box
#' @method bounding.box SpatialLinesDataFrame
#' @export
bounding.box.SpatialLinesDataFrame = bounding.box.sp

#' @rdname bounding.box
#' @method bounding.box SpatialPointsDataFrame
#' @export
bounding.box.SpatialPointsDataFrame = bounding.box.sp

#' @rdname bounding.box
#' @method bounding.box SpatialPolygonsDataFrame
#' @export
bounding.box.SpatialPolygonsDataFrame = bounding.box.sp

#' Extend coordinates of a bounding box.
#' @param bbox a list with items "xmin", "xmax", "ymin" and "ymax".
#' @param buffer amount to exceed by.
#' @return A bounding box.
#' @export extend.bbox
extend.bbox = function(bbox, buffer) {
    bbox$xmin = bbox$xmin - buffer
    bbox$xmax = bbox$xmax + buffer
    bbox$ymin = bbox$ymin - buffer
    bbox$ymax = bbox$ymax + buffer
    return(bbox)
}
