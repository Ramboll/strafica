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
    stop(
        "The function 'diff.polys' for SpatialLinesDataFrame objects has",
        "been removed from this package."
    )
}
