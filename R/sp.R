# -*- coding: us-ascii-unix -*-

#' Convert bounding box coordinates to a SpatialPolygonsDataFrame.
#' @param bbox a list with items "xmin", "xmax", "ymin" and "ymax".
#' @return A SpatialPolygonsDataFrame object.
#' @export bbox.to.sp
bbox.to.sp = function(bbox)
    polys.to.sp(bbox.to.poly(bbox))

#' Convert a grid data frame to its \pkg{sp} equivalent.
#' @param grid a grid data frame.
#' @param cell.size size of grid cells.
#' @return A \code{SpatialPolygonsDataFrame} object.
#' @seealso \code{\link[sp]{SpatialPolygonsDataFrame}}
#' @export grid.to.sp
grid.to.sp = function(grid, cell.size) {
    # Trace grid cells clockwise starting from north-west corner.
    x = unlist(lapply(grid$x, function(x) c(x - cell.size/2,
                                            x + cell.size/2,
                                            x + cell.size/2,
                                            x - cell.size/2,
                                            x - cell.size/2,
                                            NA)))

    y = unlist(lapply(grid$y, function(y) c(y + cell.size/2,
                                            y + cell.size/2,
                                            y - cell.size/2,
                                            y - cell.size/2,
                                            y + cell.size/2,
                                            NA)))

    polys = data.frame(x=x, y=y)
    grid$eid = rows.along(grid)
    polys$eid = rep(rows.along(grid), each=6)
    polys$sid = 1L
    return(polys.to.sp(polys, grid))
}

#' Convert a line data frame to its \pkg{sp} equivalent.
#' @param lines a data frame of lines.
#' @param data a data frame of per-entry attributes.
#' @param verbose a logical indicating whether or not to print out progress.
#' @return A \code{SpatialLinesDataFrame} object.
#' @seealso \code{\link[sp]{SpatialLinesDataFrame}}
#' @method lines.to.sp not.an.s3.method
#' @export lines.to.sp
lines.to.sp = function(lines, data=NULL, verbose=TRUE) {
    lines = subset(lines, !is.na(x) & !is.na(y))
    if ("sid" %nin% colnames(lines))
        lines$sid = 1L
    lines$id = classify(lines$eid, lines$sid)
    # Try to avoid topology errors with rgeos.
    # Two points are needed to form a valid line.
    stat = fold(lines, .(id), n=sum(!is.na(x)))
    lines = subset(lines, id %in% stat$id[stat$n > 1])
    if (nrow(lines) == 0)
        stop("No valid lines given")
    total = length(unique(lines$id))
    time.start = Sys.time()
    if (verbose)
        messagef("Creating %d lines...", total)
    indices = uind(lines$eid)
    shapes = mclapply.stop(seq_along(indices), function(i) {
        if (verbose & progress.due(3, parallel=TRUE))
            progress.eta(time.start, i, length(indices))
        ii = indices[[i]]
        id = as.character(lines$eid[ii][1])
        if (length(unique(lines$id[ii])) == 1)
              return(sp::Lines(list(sp::Line(
                  cbind(lines$x[ii], lines$y[ii]))), id))
        return(sp::Lines(lapply(uind(lines$id[ii]), function(jj) {
            sp::Line(cbind(lines$x[ii[jj]], lines$y[ii[jj]]))
        }), id))
    })
    if (verbose)
          progress.final(time.start)
    shapes = sp::SpatialLines(shapes)
    if (is.null(data))
        data = data.frame(eid=unique(lines$eid))
    rownames(data) = as.character(data$eid)
    return(sp::SpatialLinesDataFrame(shapes, data))
}

#' Convert a point data frame to its \pkg{sp} equivalent.
#' @param points a data frame of points.
#' @return A \code{SpatialPointsDataFrame} object.
#' @seealso \code{\link[sp]{SpatialPointsDataFrame}}
#' @method points.to.sp not.an.s3.method
#' @export points.to.sp
points.to.sp = function(points) {
    shapes = sp::SpatialPoints(cbind(points$x, points$y))
    data = points[,setdiff(colnames(points), c("x", "y")),drop=FALSE]
    shapes = sp::SpatialPointsDataFrame(shapes, data)
    shapes@data$x = points$x
    shapes@data$y = points$y
    return(shapes)
}

#' Convert a polygon data frame to its \pkg{sp} equivalent.
#' @param outlines a data frame of polygon outlines.
#' @param data a data frame of per-entry attributes.
#' @param verbose a logical indicating whether or not to print out progress.
#' @return A \code{SpatialPolygonsDataFrame} object.
#' @seealso \code{\link[sp]{SpatialPolygonsDataFrame}}
#' @export polys.to.sp
polys.to.sp = function(outlines, data=NULL, verbose=TRUE) {
    outlines = subset(outlines, !is.na(x) & !is.na(y))
    if ("sid" %nin% colnames(outlines))
        outlines$sid = 1L
    outlines$id = classify(outlines$eid, outlines$sid)
    # Try to avoid topology errors with rgeos.
    # Four points are needed to form a valid polygon.
    stat = fold(outlines, .(id), n=sum(!is.na(x)))
    outlines = subset(outlines, id %in% stat$id[stat$n > 3])
    if (nrow(outlines) == 0)
        stop("No valid polygons given")
    total = length(unique(outlines$id))
    time.start = Sys.time()
    if (verbose)
        messagef("Creating %d polygons...", total)
    indices = uind(outlines$eid)
    shapes = mclapply.stop(seq_along(indices), function(i) {
        if (verbose & progress.due(3, parallel=TRUE))
            progress.eta(time.start, i, length(indices))
        ii = indices[[i]]
        id = as.character(outlines$eid[ii][1])
        if (length(unique(outlines$id[ii])) == 1)
            return(sp::Polygons(list(sp::Polygon(cbind(
                outlines$x[ii], outlines$y[ii]
            ))), id))
        return(sp::Polygons(lapply(uind(outlines$id[ii]), function(jj) {
            sp::Polygon(cbind(outlines$x[ii[jj]], outlines$y[ii[jj]]))
        }), id))
    })
    if (verbose)
        progress.final(time.start)
    shapes = sp::SpatialPolygons(shapes, seq_along(shapes))
    if (is.null(data))
        data = data.frame(eid=unique(outlines$eid))
    rownames(data) = as.character(data$eid)
    return(sp::SpatialPolygonsDataFrame(shapes, data))
}

#' Convert a segment data frame to its \pkg{sp} equivalent.
#' @param segments a data frame of segments.
#' @return A \code{SpatialLinesDataFrame} object.
#' @seealso \code{\link[sp]{SpatialLinesDataFrame}}
#' @export segments.to.sp
segments.to.sp = function(segments) {
    segments$eid = rows.along(segments)
    segments$sid = 1L
    lines = segments.to.lines(segments)
    return(lines.to.sp(lines, segments))
}

#' Convert \pkg{sp} lines to its data frame equivalent.
#' @param shapes a \code{SpatialLinesDataFrame} object.
#' @param fun a function to apply to coordinates, e.g. \code{\link{round}}.
#' @param data.only if \code{TRUE}, don't parse geometry, return only data.
#' @param verbose a logical indicating whether or not to print out progress.
#' @return A list with two data frames: data and lines.
#' @export sp.to.lines
sp.to.lines = function(shapes, fun=identity, data.only=FALSE, verbose=TRUE) {
    bind4 = function(x)
        cbind(unlist(lapply(x, "[", TRUE, 1)),
              unlist(lapply(x, "[", TRUE, 2)),
              unlist(lapply(x, "[", TRUE, 3)),
              unlist(lapply(x, "[", TRUE, 4)))

    data = as.data.frame(shapes)
    colnames(data) = tolower(colnames(data))
    # Keep eids if it looks like they're our eids.
    if ("eid" %nin% colnames(data) || any(duplicated(data$eid)))
        data$eid = seq_along(shapes@lines)
    data = downclass(data)
    if (data.only) return(data)
    n = sum(sapply(shapes@lines, function(x) length(x@Lines)))
    time.start = Sys.time()
    if (verbose)
        messagef("Creating %d lines...", n)
    lines = bind4(mclapply.stop(seq_along(shapes@lines), function(i) {
        if (verbose & progress.due(3, parallel=TRUE))
            progress.eta(time.start, i, length(shapes@lines))
        Lines = shapes@lines[[i]]@Lines
        return(bind4(lapply(seq_along(Lines), function(sid) {
            coords = rbind(Lines[[sid]]@coords, NA)
            cbind(data$eid[i], sid, coords)
        })))
    }))
    post.gc(rm(shapes))
    if (verbose)
        progress.final(time.start)
    lines = as.data.frame(lines)
    colnames(lines) = c("eid", "sid", "x", "y")
    lines$x = fun(lines$x)
    lines$y = fun(lines$y)
    lines = downclass(lines)
    return(list(data=data, lines=lines))
}

#' Convert \pkg{sp} points to its data frame equivalent.
#' @param shapes a \code{SpatialPointsDataFrame} object.
#' @param fun a function to apply to coordinates, e.g. \code{\link{round}}.
#' @return A data frame.
#' @export sp.to.points
sp.to.points = function(shapes, fun=identity) {
    points = as.data.frame(shapes)
    post.gc(rm(shapes))
    colnames(points) = tolower(colnames(points))
    points$x = NULL
    points$y = NULL
    points = rename(points, coords.x1=x)
    points = rename(points, coords.x2=y)
    points$x = fun(points$x)
    points$y = fun(points$y)
    return(downclass(points))
}

#' Convert \pkg{sp} polygons to its data frame equivalent.
#'
#' Note that holes are marked in output, but not connected! To plot areas,
#' feed resulting polygons to \code{\link{attach.holes}}.
#' @param shapes a \code{SpatialPolygonsDataFrame} object.
#' @param fun a function to apply to coordinates, e.g. \code{\link{round}}.
#' @param area.min threshold, polygons below which to discard.
#' @param data.only if \code{TRUE}, don't parse geometry, return only data.
#' @param verbose a logical indicating whether or not to print out progress.
#' @return A list with two data frames: data and outlines.
#' @export sp.to.polys
sp.to.polys = function(shapes, fun=identity, area.min=NULL, data.only=FALSE, verbose=TRUE) {
    bind5 = function(x)
        cbind(unlist(lapply(x, "[", TRUE, 1)),
              unlist(lapply(x, "[", TRUE, 2)),
              unlist(lapply(x, "[", TRUE, 3)),
              unlist(lapply(x, "[", TRUE, 4)),
              unlist(lapply(x, "[", TRUE, 5)))

    data = as.data.frame(shapes)
    colnames(data) = tolower(colnames(data))
    # Keep eids if it looks like they're our eids.
    if ("eid" %nin% colnames(data) || any(duplicated(data$eid)))
        data$eid = seq_along(shapes@polygons)
    data$x = slots(shapes@polygons, "labpt")[1,]
    data$y = slots(shapes@polygons, "labpt")[2,]
    data$x = fun(data$x)
    data$y = fun(data$y)
    data$area = slots(shapes@polygons, "area")
    data = downclass(data)
    if (data.only) return(data)
    n = sum(sapply(shapes@polygons, function(x) length(x@Polygons)))
    time.start = Sys.time()
    if (verbose)
        messagef("Creating %d polygons...", n)
    polys = bind5(mclapply.stop(seq_along(shapes@polygons), function(i) {
        if (verbose & progress.due(3, parallel=TRUE))
            progress.eta(time.start, i, length(shapes@polygons))
        Polygons = shapes@polygons[[i]]@Polygons
        if (is.numeric(area.min) && area.min > 0) {
            # Discard all polygons below area.min.
            keep = slots(Polygons, "area") >= area.min
            Polygons = subset(Polygons, keep)
            if (length(Polygons) == 0) return(NULL)
        }
        return(bind5(lapply(seq_along(Polygons), function(sid) {
            coords = rbind(Polygons[[sid]]@coords, NA)
            cbind(data$eid[i], sid, coords, Polygons[[sid]]@hole)
        })))
    }))
    post.gc(rm(shapes))
    if (verbose) {
        progress.final(time.start)
    }
    polys = as.data.frame(polys)
    colnames(polys) = c("eid", "sid", "x", "y", "hole")
    polys$hole = as.logical(polys$hole)
    polys$x = fun(polys$x)
    polys$y = fun(polys$y)
    polys = downclass(polys)
    return(list(data=data, outlines=polys))
}
