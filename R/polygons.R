# -*- coding: us-ascii-unix -*-

#' Connect holes to their outer polygons.
#' @param polys a data frame of polygons.
#' @return A data frame of polygons.
#' @export attach.holes
attach.holes = function(polys) {
    if (!any(polys$hole)) return(polys)
    polys = subset(polys, !(hole & is.na(x)))
    id = classify(polys$eid, polys$sid)
    hh = which(polys$hole)
    oo = which(!polys$hole)
    # Find the corresponding outer polygon for each holepoint and
    # in case of boundary points take the most common for each hole.
    points = find.containers(polys$x[hh], polys$y[hh], polys[oo,])
    points = transform(points, id=id[hh], n=1L)
    points = rename(points, eid=peid, sid=psid)
    points = fold(points, .(id, peid, psid), n=sum(n))
    points = arrange(points, desc(n))
    points = subset(points, !duplicated(id))
    rows = which(!polys$hole)
    time.start = Sys.time()
    messagef("Attaching %d holes...", nrow(points))
    for (i in rows.along(points)) {
        # Attach hole to the closest point in parent or a hole already
        # processed earlier. This is not necessary to render polygons
        # correctly, but should be clearer and easier to debug.
        if (progress.due(3, parallel=FALSE))
            progress.eta(time.start, i, nrow(points))
        h = which(id == points$id[i])
        p = which(polys$eid == points$peid[i] & polys$sid == points$psid[i])
        dist = with(polys, eucd2(x[h[1]], y[h[1]], x[p], y[p]))
        m = min(which(rows == p[which.min(dist)]))
        rows = c(rows[1:m], h, rows[m:length(rows)])
        polys$eid[h] = points$peid[i]
        polys$sid[h] = points$psid[i]
    }
    progress.final(time.start)
    polys = polys[rows,,drop=FALSE]
    rownames(polys) = rows.along(polys)
    return(polys)
}

#' Construct polygon approximations of circles.
#' @param xc x-coordinates of circle centroids.
#' @param yc y-coordinates of circle centroids.
#' @param radius radii of circles.
#' @param n amount of points in one circle polygon.
#' @return A data frame of polygons.
#' @export circles
circles = function(xc, yc, radius, n=100) {
    circles = data.frame(eid=c(), sid=c(), x=c(), y=c())
    theta = seq(2 * pi, 0, length.out=n+1)
    ncircles = max(length(xc), length(yc), length(radius))
    xc = rep(xc, length.out=ncircles)
    yc = rep(yc, length.out=ncircles)
    radius = rep(radius, length.out=ncircles)
    return(rbind_all(lapply(seq_along(xc), function(i) {
        x = c(radius[i] * cos(theta) + xc[i], NA)
        y = c(radius[i] * sin(theta) + yc[i], NA)
        return(data.frame(eid=1, sid=i, x=x, y=y))
    })))
}

#' Trace grid boundary to polygon outlines and areas.
#' @param grid a grid data frame.
#' @param cell.size size of grid cells.
#' @param by optional columns to group \code{grid} by.
#' @return A list with three data frames: data, outlines and areas.
#' @export trace.grid
trace.grid = function(grid, cell.size, by=NULL) {
    if (is.null(by)) {
        messagef("Tracing grid with %d cells...", nrow(grid))
        # It would be faster to convert to a grid object (SpatialPixels,
        # SpatialGrid, etc.), but it is not obvious how to trace that;
        # gUnion functions want SpatialPolygons.
        shapes = grid.to.sp(grid, cell.size)
        shapes = rgeos::gUnaryUnion(shapes)
        n = length(shapes@polygons)
        data = data.frame(dummy=rep(1L, n))
        shapes = sp::SpatialPolygonsDataFrame(shapes, data=data)
        polys = sp.to.polys(shapes)
        polys$data$dummy = NULL
        polys$areas = attach.holes(polys$outlines)
    } else {
        id = do.call(classify, grid[,by,drop=FALSE])
        ids = sort(unique(id))
        messagef("Tracing grid with %d cells in %d parts...",
                 nrow(grid), length(ids))

        # grid.to.sp uses mclapply, so this is a second level of
        # parallelization, but usually it shouldn't be a problem
        # and should speed up gUnaryUnion and sp conversions.
        polys = mclapply.stop(seq_along(ids), function(i) {
            grid = grid[which(id == ids[i]),,drop=FALSE]
            polys = suppressMessages(trace.grid(grid, cell.size))
            polys$data[,by] = rep(grid[1,by], each=nrow(polys$data))
            polys$data$eid = polys$outlines$eid = polys$areas$eid = i
            return(polys)
        })
        data = rbind_all(lapply(polys, getElement, "data"))
        outlines = rbind_all(lapply(polys, getElement, "outlines"))
        areas = rbind_all(lapply(polys, getElement, "areas"))
        polys = list(data=data, outlines=outlines, areas=areas)
    }
    return(polys)
}

#' Find discrete Voronoi regions.
#'
#' For each area with centroids \code{cx}, \code{cy} find out which
#' of the points \code{px}, \code{py} is closest to the centroid.
#' @param cx X-coordinates of area centroids.
#' @param cy Y-coordinates of area centroids.
#' @param px X-coordinates of points.
#' @param py Y-coordinates of points.
#' @param pid point IDs.
#' @return A vector of point IDs for each area.
#' @export voronoi.area
voronoi.area = function(cx, cy, px, py, pid) {
    id = NA
    dist = Inf
    time.start = Sys.time()
    messagef("Finding Voronoi regions for %d points...", length(px))
    for (i in seq_along(px)) {
        if (progress.due(3, parallel=FALSE))
            progress.eta(time.start, i, length(px))
        idist = eucd2(px[i], py[i], cx, cy)
        m = which(idist < dist)
        id[m] = pid[i]
        dist[m] = idist[m]
    }
    progress.final(time.start)
    return(id)
}
