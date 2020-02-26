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
    polys@data$.aggregate.id = do.call(
        classify, polys@data[,by,drop=FALSE])
    fulldata = polys@data
    polys = rgeos::gUnaryUnion(polys, polys@data$.aggregate.id)
    aggdata = data.frame(.aggregate.id=slots(polys@polygons, "ID"))
    fulldata$.aggregate.id = as.character(fulldata$.aggregate.id)
    fulldata = fulldata[,c(".aggregate.id", by)]
    aggdata = leftjoin(aggdata, fulldata, by=".aggregate.id")
    if (!is.null(data))
        aggdata = leftjoin(aggdata, data, by=by)
    return(sp::SpatialPolygonsDataFrame(polys, data=aggdata))
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
    sets = list(...)
    if (length(sets) == 0) return(NULL)
    for (i in seq_along(sets)[-1])
        sets[[1]] = rgeos::gUnion(sets[[1]], sets[[i]])
    polys = rgeos::gUnaryUnion(sets[[1]])
    data = data.frame(eid=1L)
    return(sp::SpatialPolygonsDataFrame(polys, data=data))
}
