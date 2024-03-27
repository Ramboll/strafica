# -*- coding: us-ascii-unix -*-

#' Aggregate polygon geometry and data by data column values.
#'
#' \code{aggregate.polys} does not aggregate data, only joins it.
#' The caller is expected to separately aggregate one's data, see examples.
#' @param polys polygons as either a list as returned by
#' \code{\link{sp.to.polys}} or SpatialPolygonsDataFrames.
#' @param by names of data columns to aggregate by.
#' @param data aggregated data to join to result.
#' @return Polygons in same format as given.
#' @seealso \code{\link{union.polys}}
#' @examples
#' \dontrun{
#' data = fold(municipalities$data, .(province), area=sum(area))
#' provinces = aggregate.polys(municipalities, by="province", data=data)
#' }
#' @rdname aggregate.polys
#' @export
aggregate.polys = function(polys, by, data=NULL)
    UseMethod("aggregate.polys", polys)

#' @rdname aggregate.polys
#' @method aggregate.polys list
#' @export
aggregate.polys.list = function(polys, by, data=NULL) {
    polys = polys.to.sp(polys$outlines, polys$data)
    polys = aggregate.polys(polys, by, data)
    polys = sp.to.polys(polys)
    polys$areas = attach.holes(polys$outlines)
    return(polys)
}

#' @rdname aggregate.polys
#' @method aggregate.polys SpatialPolygonsDataFrame
#' @export
aggregate.polys.SpatialPolygonsDataFrame = function(polys, by, data=NULL) {
    stop(
        "The function 'aggregate.polys' for SpatialLinesDataFrame objects has",
        "been removed from this package."
    )
}

#' Combine sets of polygons to form one polygon.
#' @param ... sets of polygons as either outline data frames, lists as
#' returned by \code{\link{sp.to.polys}} or SpatialPolygonsDataFrames.
#' @return A polygon in same format as given.
#' @seealso \code{\link{aggregate.polys}}
#' @rdname union.polys
#' @export
union.polys = function(...)
    UseMethod("union.polys")

#' @rdname union.polys
#' @method union.polys data.frame
#' @export
union.polys.data.frame = function(...) {
    sets = list(...)
    if (length(sets) == 0) return(NULL)
    sps = lapply(sets, polys.to.sp)
    polys = do.call(union.polys, sps)
    return(sp.to.polys(polys)$outlines)
}

#' @rdname union.polys
#' @method union.polys list
#' @export
union.polys.list = function(...) {
    sets = list(...)
    if (length(sets) == 0) return(NULL)
    sps = lapply(sets, function(x) polys.to.sp(x$outlines))
    polys = do.call(union.polys, sps)
    polys = sp.to.polys(polys)
    polys$areas = attach.holes(polys$outlines)
    return(polys)
}

#' @rdname union.polys
#' @method union.polys SpatialPolygonsDataFrame
#' @export
union.polys.SpatialPolygonsDataFrame = function(...) {
    stop(
        "The function 'union.polys' for SpatialLinesDataFrame objects has",
        "been removed from this package."
    )
}
