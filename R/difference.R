# -*- coding: us-ascii-unix -*-

#' Remove areas of second polygons from first.
#'
#' Polygons can be given as either outline data frames, lists as
#' returned by \code{\link{sp.to.polys}} or SpatialPolygonsDataFrames,
#' but both \code{polys1} and \code{polys2} should be in the same format.
#' @param polys1 Polygons to operate on.
#' @param polys2 Polygons to remove from \code{polys1}.
#' @return Polygons in same format as given.
#' @rdname diff.polys
#' @export
diff.polys = function(polys1, polys2)
    UseMethod("diff.polys", polys1)

#' @rdname diff.polys
#' @method diff.polys data.frame
#' @export
diff.polys.data.frame = function(polys1, polys2) {
    sp1 = polys.to.sp(polys1)
    sp2 = polys.to.sp(polys2)
    polys = diff.polys(sp1, sp2)
    return(sp.to.polys(polys)$outlines)
}

#' @rdname diff.polys
#' @method diff.polys list
#' @export
diff.polys.list = function(polys1, polys2) {
    sp1 = polys.to.sp(polys1$outlines)
    sp2 = polys.to.sp(polys2$outlines)
    polys = sp.to.polys(diff.polys(sp1, sp2))
    polys$areas = attach.holes(polys$outlines)
    return(polys)
}

#' @rdname diff.polys
#' @method diff.polys SpatialPolygonsDataFrame
#' @export
diff.polys.SpatialPolygonsDataFrame = function(polys1, polys2) {
    data = as.data.frame(polys1)
    polys1 = rgeos::createSPComment(polys1)
    polys2 = rgeos::createSPComment(polys2)
    # rgeos is very strict about polygons having correct topology;
    # try to fix possible topology errors by buffering.
    polys1 = rgeos::gBuffer(polys1, width=0, byid=TRUE)
    polys2 = rgeos::gBuffer(union.polys(polys2), width=0, byid=TRUE)
    sp::proj4string(polys1) = ""
    sp::proj4string(polys2) = ""
    # XXX: drop_lower_td=TRUE drops *everything* (rgeos 0.3-8).
    polys = rgeos::gDifference(polys1, polys2, byid=TRUE, drop_lower_td=FALSE)
    if (methods::is(polys, "SpatialCollections"))
        polys = polys@polyobj
    # gDifference returns combined IDs, of which we want the first,
    # which allows us to merge the data back that gDifference lost.
    id = gsub(" .*$", "", slots(polys@polygons, "ID"))
    for (i in seq_along(polys@polygons))
        polys@polygons[[i]]@ID = id[i]
    data = data[which(rownames(data) %in% id),,drop=FALSE]
    return(sp::SpatialPolygonsDataFrame(polys, data))
}
