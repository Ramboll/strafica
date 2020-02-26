# -*- coding: us-ascii-unix -*-

#' Split data frame and calculate group-wise summaries.
#' @param df data frame to summarize.
#' @param by columns to split by, either quoted or strings.
#' @param ... one or more keyword arguments with function calls
#' of summaries to calculate, e.g. \code{xmin=min(x), xmax=max(x)}.
#' @return A data frame with one row for each unique combination of \code{by}
#' and columns \code{by} and \code{...} in sorted order of \code{by}.
#' @export bigfold
bigfold = function(df, by, ...) {
    .deprecated("bigfold is deprecated, please use fold instead.")
    return(fold(df, by, ...))
}

#' Join two data frames together.
#' @param x a data frame.
#' @param y a data frame.
#' @param by names of columns to join by.
#' @param size ignored.
#' @return A data frame with as many rows as \code{x}.
#' @export bigjoin
bigjoin = function(x, y, by=intersect(names(x), names(y)), size=-1) {
    .deprecated("bigjoin is deprecated, please use leftjoin instead.")
    return(leftjoin(x, y, by))
}

#' Join two data frames together.
#' @param x a data frame.
#' @param y a data frame.
#' @param by names of columns to join by.
#' @param size ignored.
#' @return A data frame with as many rows as \code{x}.
#' @export bigjoin.via.file
bigjoin.via.file = function(x, y, by=intersect(names(x), names(y)), size=-1) {
    .deprecated("bigjoin.via.file is deprecated, please use leftjoin instead.")
    return(leftjoin(x, y, by))
}

# Show a deprecation message only once per R session.
.deprecated = local({
    shown = character(0)
    function(text, ...) {
        if (text %in% shown) return(NULL)
        shown <<- c(shown, text)
        return(message(text, ...))
    }
})

#' Combine data frames by row.
#' @param ... data frames.
#' @return A data frame.
#' @export mcrbind
mcrbind = function(...) {
    .deprecated("mcrbind is deprecated, please use rbind_list instead.")
    return(rbind_list(...))
}

#' Combine data frames by row.
#' @param ... data frames.
#' @return A data frame.
#' @export rbind.fill
rbind.fill = function(...) {
    .deprecated("rbind.fill is deprecated, please use rbind_list instead.")
    return(rbind_list(...))
}

#' Combine data frames by row.
#' @param ... data frames.
#' @return A data frame.
#' @export rbind.pad
rbind.pad = function(...) {
    .deprecated("rbind.pad is deprecated, please use rbind_list instead.")
    return(rbind_list(...))
}

#' Combine data frames by row.
#' @param ... data frames.
#' @return A data frame.
#' @export rbind.via.file
rbind.via.file = function(...) {
    .deprecated("rbind.via.file is deprecated, please use rbind_list instead.")
    return(rbind_list(...))
}

#' Remove object from caller's environment and collect garbage.
#' @param ... the objects to be removed, as names (unquoted)
#' or character strings (quoted).
#' @param list a character vector naming objects to be removed.
#' @export rm.gc
rm.gc = function(..., list=character(0)) {
    .deprecated("rm.gc is deprecated, please use post.gc(rm(...)) instead.")
    dots = match.call(expand.dots=FALSE)$...
    names = unlist(lapply(dots, as.character))
    rm(list=c(names, list), envir=parent.frame())
    return(invisible(gc(verbose=FALSE)))
}

#' Angle for segment labels.
#' @param segments a data frame of segments.
#' @return A vector of angles, in degrees.
#' @export segment.label.angle
segment.label.angle = function(segments) {
    .deprecated(pasten(
        "segment.label.angle is deprecated, please use segment.labels instead.",
        "e.g. 'labels = segment.labels(volumes, polys)'."))
    with(segments, rad2deg(atan((jy-iy)/(jx-ix))))
}

#' Label points for scaled segments.
#' @param polys a data frame of segment polygons as returned
#' by \code{\link{scale.segments}}.
#' @param vjust.above vjust specification for labels above, logically <= 0.
#' @param vjust.below vjust specification for labels below, logically >= 1.
#' @return A data frame of points.
#' @export segment.label.points
segment.label.points = function(polys, vjust.above=-0.1, vjust.below=1.1) {
    .deprecated(pasten(
        "segment.label.points is deprecated, please use segment.labels instead.",
        "e.g. 'labels = segment.labels(volumes, polys)'."))
    polys$vjust_above = vjust.above
    polys$vjust_below = vjust.below
    fold(polys, .(eid),
         vjust=ifelse(mean(y[3:4]) > mean(y[1:2]),
                      vjust_above,
                      vjust_below),

         x=mean(x[3:4]),
         y=mean(y[3:4]))

}
