# -*- coding: us-ascii-unix -*-

#' Connect consecutive line objects.
#' @param lines a list as returned by \code{\link{sp.to.lines}}.
#' @param by names of columns in data that must match in order to join.
#' @param threshold maximum allowed distance of line-ends.
#' @return A list of lines.
#' @export connect.consecutive.lines
connect.consecutive.lines = function(lines, by, threshold=0) {
    # XXX: This is way too slow for large linesets.
    # We need some sensible way to parallelize the problem.
    uniqa = function(x) which(!duplicated(x, fromLast=FALSE))
    uniqz = function(x) which(!duplicated(x, fromLast=TRUE))
    lines$lines$id = classify(lines$lines$eid, lines$lines$sid)
    ii = rows.along(lines$lines)
    lines$lines$ap = ii %in% (uniqa(lines$lines$id))
    lines$lines$zp = ii %in% (uniqz(lines$lines$id)-1)
    data = lines$data[, c("eid", by), drop=FALSE]
    lines$lines = leftjoin(lines$lines, data, by="eid")
    stat = fold(lines$lines, .(id), n=length(x)-1)
    lines$lines = leftjoin(lines$lines, stat, by="id")
    lines$lines$left = lines$lines$zp
    lines$lines$order = rows.along(lines$lines)
    total = 0
    time.start = Sys.time()
    messagef("Considering connections for %d endpoints...",
             sum(lines$lines$left))

    while (any(lines$lines$left)) {
        # Begin with the endpoint of the longest remaining line
        # in order to avoid misconnecting short stubs.
        i = which(lines$lines$left)
        i = i[which.max(lines$lines$n[i])]
        lines$lines$left[i] = FALSE
        if (progress.due(3, parallel=FALSE)) {
            n = sum(lines$lines$zp) - sum(lines$lines$left)
            progress.eta(time.start, n, sum(lines$lines$zp))
        }
        ii = which(lines$lines$id == lines$lines$id[i])
        m = which(lines$lines$ap)
        xs = lines$lines$x
        ys = lines$lines$y
        if (threshold == 0) {
            m = m[which(xs[m] == xs[i])]
            m = m[which(ys[m] == ys[i])]
        } else {
            m = m[which(abs(xs[m] - xs[i]) <= threshold)]
            m = m[which(abs(ys[m] - ys[i]) <= threshold)]
        }
        m = setdiff(m, ii)
        m = m[unlist(lapply(m, function(j) {
            # Require that by columns of data be equal.
            identical(lines$lines[i,by], lines$lines[j,by])
        }))]
        if (length(m) == 0) next
        total = total + 1
        # Duplicate data over NA row to avoid slow live updates.
        # These duplicate rows as well as the first in m are
        # removed in one go after the loop.
        stopifnot(is.na(lines$lines$x[i+1]))
        lines$lines$x[i+1] = lines$lines$x[i]
        lines$lines$y[i+1] = lines$lines$y[i]
        # Connect lines to the longest possible continuation found.
        # XXX: Maybe we should use the smallest difference in angle?
        m = m[which.max(lines$lines$n[m])]
        mm = which(lines$lines$id == lines$lines$id[m])
        n2 = lines$lines$n[ii][1] + lines$lines$n[mm][1]
        order2 = with(lines$lines, max(order[ii]) + order[mm])
        lines$lines$zp[ii] = FALSE
        lines$lines$n[ii] = n2
        lines$lines$eid[mm] = lines$lines$eid[i]
        lines$lines$sid[mm] = lines$lines$sid[i]
        lines$lines$id[mm] = lines$lines$id[i]
        lines$lines$ap[mm] = FALSE
        lines$lines$n[mm] = n2
        lines$lines$order[mm] = order2
    }
    lines$lines = arrange(lines$lines, eid, sid, order)
    lines$lines = dedup(lines$lines, eid, sid, x, y)
    z = which(!duplicated(lines$lines$id, fromLast=TRUE))
    stopifnot(all(is.na(lines$lines$x[z])))
    messagef("Connected %d/%d lines.", total, nrow(lines$data))
    lines$data = subset(lines$data, eid %in% lines$lines$eid)
    lines$lines = pick(lines$lines, eid, sid, x, y)
    return(lines)
}

#' Reverse direction of opposing segments.
#'
#' If there are two segments between a pair of nodes going in opposite
#' directions, flip either one of those so that they both go in the same
#' direction. The usual use case for this is prior to aggregation, where only
#' one value is desired between a pair of nodes.
#' @param segments a data frame of segments.
#' @return Data frame \code{segments} with some segments flipped.
#' @export flip.opposing.segments
flip.opposing.segments = function(segments) {
    ixy = paste(segments$ix, segments$iy, sep="@")
    jxy = paste(segments$jx, segments$jy, sep="@")
    xy.min = pmin(ixy, jxy)
    xy.max = pmax(ixy, jxy)
    segments$ix = gsub("^(\\S+)@(\\S+)$", "\\1", xy.min, perl=TRUE)
    segments$iy = gsub("^(\\S+)@(\\S+)$", "\\2", xy.min, perl=TRUE)
    segments$jx = gsub("^(\\S+)@(\\S+)$", "\\1", xy.max, perl=TRUE)
    segments$jy = gsub("^(\\S+)@(\\S+)$", "\\2", xy.max, perl=TRUE)
    segments$ix = as.numeric(segments$ix)
    segments$iy = as.numeric(segments$iy)
    segments$jx = as.numeric(segments$jx)
    segments$jy = as.numeric(segments$jy)
    return(downclass(segments))
}

#' Flip direction of segments.
#'
#' @param segments a data frame of segments.
#'
#' @return a data frame of segments, with endpoints flipped.
#' @export flip_segments
flip_segments = function(segments) {
    coords = c("ix", "iy", "jx", "jy")
    if (any(coords %nin% names(segments))) {
        stop("Object is not a segment!")
    }
    segments = transform(segments, ix = jx, iy = jy, jx = ix, jy = iy)
    return(segments)
}

#' Intersection points of two lines.
#' @param ix1 X-coordinate of first point along line 1.
#' @param iy1 Y-coordinate of first point along line 1.
#' @param jx1 X-coordinate of second point along line 1.
#' @param jy1 Y-coordinate of second point along line 1.
#' @param ix2 X-coordinate of first point along line 2.
#' @param iy2 Y-coordinate of first point along line 2.
#' @param jx2 X-coordinate of second point along line 2.
#' @param jy2 Y-coordinate of second point along line 2.
#' @return A list of X and Y coordinates of intersection points.
#' @references \url{http://mathworld.wolfram.com/Line-LineIntersection.html}
#' @export lineint
lineint = function(ix1, iy1, jx1, jy1, ix2, iy2, jx2, jy2) {
    for (name in c("ix1", "iy1", "jx1", "jy1", "ix2", "iy2", "jx2", "jy2"))
        assign(name, as.numeric(get(name)))
    x = (((ix1*jy1-iy1*jx1)*(ix2-jx2)-(ix1-jx1)*(ix2*jy2-iy2*jx2))
         / ((ix1-jx1)*(iy2-jy2)-(iy1-jy1)*(ix2-jx2)))
    y = (((ix1*jy1-iy1*jx1)*(iy2-jy2)-(iy1-jy1)*(ix2*jy2-iy2*jx2))
         / ((ix1-jx1)*(iy2-jy2)-(iy1-jy1)*(ix2-jx2)))
    return(list(x=x, y=y))
}

#' Convert lines to segments.
#' @param lines a data frame of lines.
#' @param data a data frame of per-entry attributes.
#' @return A data frame of segments.
#' @method lines.to.segments not.an.s3.method
#' @export lines.to.segments
lines.to.segments = function(lines, data = NULL) {
    lines = subset(lines, !is.na(x) & !is.na(y))
    id = classify(lines$eid, lines$sid)
    first = !duplicated(id, fromLast=FALSE)
    last = !duplicated(id, fromLast=TRUE)
    segments = data.frame(eid=lines$eid[!last],
                          sid=lines$sid[!last],
                          ix=lines$x[!last],
                          iy=lines$y[!last],
                          jx=lines$x[!first],
                          jy=lines$y[!first])

    skip = c(colnames(segments), "x", "y")
    for (name in setdiff(colnames(lines), skip))
        segments[,name] = lines[!last, name]
    if (!is.null(data)) {
        segments = leftjoin(segments, data, by = "eid")
    }
    return(segments)
}

#' Convert segments to lines
#' @param segments a data frame of segments.
#' @return A data frame of lines.
#' @export segments.to.lines
segments.to.lines = function(segments) {
    if (nrow(segments) == 0)
        return(data.frame(eid=0L, sid=0L, x=0L, y=0L)[FALSE,])
    eid = rep(segments$eid, each=2)
    sid = rep(segments$sid, each=2)
    lines = data.frame(eid=eid, sid=sid, x=NA, y=NA)
    lines$x[seq(1, nrow(lines), 2)] = segments$ix
    lines$y[seq(1, nrow(lines), 2)] = segments$iy
    lines$x[seq(2, nrow(lines), 2)] = segments$jx
    lines$y[seq(2, nrow(lines), 2)] = segments$jy
    skip = c(colnames(lines), "ix", "iy", "jx", "jy")
    for (name in setdiff(colnames(segments), skip))
        lines[,name] = rep(segments[,name], each=2)
    lines = subset(lines, c(1, diff(classify(eid, sid, x, y))) != 0)
    id = classify(lines$eid, lines$sid)
    m = which(!duplicated(id, fromLast=TRUE))
    lines = lines[sort(c(rows.along(lines), m)),]
    id = classify(lines$eid, lines$sid)
    m = which(!duplicated(id, fromLast=TRUE))
    lines$x[m] = lines$y[m] = NA
    rownames(lines) = rows.along(lines)
    return(lines)
}

#' Discard duplicated lines.
#'
#' A common use for \code{uniquify.lines} is with area boundaries, where the
#' boundary between two areas exists in two shapes and is often undesirable to
#' be plotted twice as it ruins linetype and alpha use.
#' @param lines a data frame of lines.
#' @return A subset of \code{lines}.
#' @export uniquify.lines
uniquify.lines = function(lines) {
    segments = lines.to.segments(lines)
    flipped = flip.opposing.segments(segments)
    segments$keep = with(flipped, !duplicated(classify(ix, iy, jx, jy)))
    # Renumber shapes, since segment operations may have
    # broken line shapes into smaller pieces.
    sub = as.integer(as.factor(cumsum(!segments$keep)))
    segments$sid = classify(segments$sid, sub)
    segments = subset(segments, keep)
    segments = subset(segments, ix != jx | iy != jy)
    segments$keep = NULL
    return(segments.to.lines(segments))
}
