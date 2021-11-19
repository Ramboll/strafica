# -*- coding: us-ascii-unix -*-

#' Aggregate values of equidirectional parallel segments.
#'
#' \code{aggregate.parallel.segments} detects which segments are equidirectional
#' and parallel within given tolerance (e.g. sidewalks on opposite sides
#' of a street) and will aggregate values on such segments, placing aggregate
#' values on both segments! For plotting, these duplicates (among other overlaps)
#' can be removed with \code{\link{discard.segment.labels}}.
#' @param segments a data frame of segments.
#' @param name name of column holding value in \code{segments}.
#' @param radius distance (in coordinate units) within which
#' to find parellel segments.
#' @param angle maximum allowed angle difference (in degrees).
#' @param by columns in \code{segments} to parallelize over.
#' Each by-wise part will be aggregated independent of each other.
#' @return Data frame \code{segments} with column "combined" added.
#' @method aggregate.parallel.segments not.an.s3.method
#' @export aggregate.parallel.segments
aggregate.parallel.segments = function(segments, name, radius, angle, by=NULL) {
    if (!is.null(by)) {
        # Call recursively for each group.
        groups = uind(do.call(classify, segments[,by,drop=FALSE]))
        return(rbind_all(mclapply.stop(groups, function(ii) {
            aggregate.parallel.segments(segments[ii,,drop=FALSE],
                                        name=name,
                                        radius=radius,
                                        angle=angle,
                                        by=NULL)

        })))
    }
    segments$combined = NA
    segments = arrange(segments, desc(segments[,name]))
    cx = rowMeans(pick(segments, ix, jx))
    cy = rowMeans(pick(segments, iy, jy))
    xdir = sign(segments$jx - segments$ix)
    ydir = sign(segments$jy - segments$iy)
    time.start = Sys.time()
    message("Aggregating parallel segments...")
    while (any(is.na(segments$combined))) {
        m = which(is.na(segments$combined))
        i = min(m)
        if (progress.due(3, parallel=FALSE))
            progress.eta(time.start, i, nrow(segments))
        segments$combined[i] = FALSE
        dist = eucd(cx[i], cy[i], cx[m], cy[m])
        m = m[which(dist < radius)]
        if (length(m) < 2) next
        m = m[with(segments, jx[m] != ix[i] | jy[m] != iy[i])]
        m = m[with(segments, ix[m] != jx[i] | iy[m] != jy[i])]
        m = m[abs(segments$angle[m] - segments$angle[i]) < angle]
        m = m[xdir[m] == xdir[i] & ydir[m] == ydir[i]]
        if (length(m) < 2) next
        segments$combined[m] = TRUE
        segments[m,name] = sum(segments[m,name])
    }
    progress.final(time.start)
    return(segments)
}

#' Discard overlapping segment value labels.
#'
#' Remove overlapping labels so that within given radius only one point
#' with the greatest value is kept.
#' @param segments a data frame of segments.
#' @param name name of column holding value in \code{segments}.
#' @param radius distance (in coordinate units) within to clear of overlaps.
#' @return A subset of \code{segments}.
#' @export discard.segment.labels
discard.segment.labels = function(segments, name, radius) {
    segments = arrange(segments, desc(abs(segments[[name]])))
    lengths = with(segments, eucd(ix, iy, jx, jy))
    angle = with(segments, rad2deg(atan2(jy-iy, jx-ix)))
    cx = rowMeans(pick(segments, ix, jx))
    cy = rowMeans(pick(segments, iy, jy))
    keep = rep(NA, nrow(segments))
    time.start = Sys.time()
    message("Discarding overlapping labels...")
    while (any(is.na(keep))) {
        m = which(is.na(keep))
        i = min(m)
        # Speed up search by limiting to area around i.
        # A too small value could cause opposite labels
        # to not be found for links with high volumes.
        m = m[which(abs(cx[m] - cx[i]) < radius*10)]
        m = m[which(abs(cy[m] - cy[i]) < radius*10)]
        if (progress.due(3, parallel=FALSE))
            progress.eta(time.start, i, nrow(segments))
        # Keep all opposite labels not yet obscured
        # by any previously found greater label.
        angled = (angle[m] - angle[i] + 360) %% 360
        tol = min(lengths[i]/2, radius/10)
        opp = m[which(abs(cx[m] - cx[i]) < tol)]
        opp = m[which(abs(cy[m] - cy[i]) < tol)]
        opp = m[which(angled > 170 & angled < 190)]
        opp = if (length(opp) > 0) opp[1] else i
        keep[c(i, opp)] = TRUE
        m = setdiff(m, c(i, opp))
        dist1 = with(segments, eucd(x[i], y[i], x[m], y[m]))
        dist2 = with(segments, eucd(x[opp], y[opp], x[m], y[m]))
        keep[m[which(dist1 < radius | dist2 < radius)]] = FALSE
    }
    progress.final(time.start)
    return(segments[keep,,drop=FALSE])
}

#' Horizontal scalebar for \pkg{ggplot2}.
#' @param breaks a data frame with columns "x" and "y".
#' @param dy tick height relative to entire width.
#' @param ... passed to \code{\link[ggplot2]{geom_segment}}.
#' @return Object from \code{\link[ggplot2]{geom_segment}}.
#' @export hscale_segment
hscale_segment = function(breaks, dy=1/50, ...) {
    y = unique(breaks$y)
    stopifnot(length(y) == 1)
    dx = max(breaks$x) - min(breaks$x)
    dy = dy * dx
    hscale = data.frame(ix=min(breaks$x), iy=y, jx=max(breaks$x), jy=y)
    vticks = data.frame(ix=breaks$x, iy=y-dy, jx=breaks$x, jy=y+dy)
    df = rbind_list(hscale, vticks)
    return(ggplot2::geom_segment(data=df,
                                 aes(x=ix, xend=jx, y=iy, yend=jy),
                                 ...))

}

#' Horizontal scalebar text for \pkg{ggplot2}.
#' @param breaks a data frame with columns "x", "y" and "label".
#' @param dy text offset relative to entire width.
#' @param ... passed to \code{\link[ggplot2]{geom_text}}.
#' @return Object from \code{\link[ggplot2]{geom_text}}.
#' @export hscale_text
hscale_text = function(breaks, dy=2/50, ...) {
    dx = max(breaks$x) - min(breaks$x)
    dy = dy * dx
    breaks$y = breaks$y + dy
    return(ggplot2::geom_text(data=breaks,
                              aes(x=x, y=y, label=label),
                              hjust=0.5,
                              vjust=0,
                              ...))

}

#' Build numeric range labels for a plot legend.
#' @param a a numeric or character vector of lower bounds of ranges.
#' @param b a numeric or character vector of upper bounds of ranges.
#' @param sep separator between lower and upper bounds.
#' @return A quoted expression (or whatever). Just pass it as a
#' labels argument to a \pkg{ggplot2} scale function.
#' @method range.labels not.an.s3.method
#' @export range.labels
range.labels = function(a, b, sep="-") {
    na = max(nchar(a)) - nchar(a)
    nb = max(nchar(b)) - nchar(b)
    a0 = sapply(na, function(x) paste(rep("0", x), collapse=""))
    b0 = sapply(nb, function(x) paste(rep("0", x), collapse=""))
    if (sep == "-") {
        # The default sep argument cannot (?) be a Unicode dash since that
        # does not in documentation compile to PDF using Rd2pdf. Hence,
        # default to an ASCII dash, but actually use an en dash.
        sep = "\u2013"
    } else if (sep == "...") {
        sep = " . . . "
    }
    return(sapply(seq_along(a), function(i) {
        as.expression(bquote(phantom(.(a0[i])) * .(a[i]) ~ .(sep) ~
                                 phantom(.(b0[i])) * .(b[i])))

    }))
}

#' Save a \pkg{ggplot2} plot.
#' @param plot a \pkg{ggplot2} plot object.
#' @param fname name of file to produce.
#' @param ... passed to \code{\link[ggplot2]{ggsave}}.
#' @export save.plot
save.plot = function(plot, fname, ...) {
    message("This function is deprecated, please use ggplot2::ggsave()")
    if (grepl("\\.pdf$", fname)) {
        return(save.plot.pdf(plot, fname, ...))
    }
    return(ggplot2::ggsave(fname, plot, ...))
}

#' Save a \pkg{ggplot2} plot as PDF.
#' @param plot a \pkg{ggplot2} plot object.
#' @param fname name of PDF-file to produce.
#' @param family name of font family to use.
#' @param device device to use.
#' @param ... passed to \code{\link[ggplot2]{ggsave}}.
#' @export save.plot.pdf
save.plot.pdf = function(plot, fname,
                         device=grDevices::cairo_pdf, ...)
    # Default to using 'cairo_pdf', which embeds fonts and handles
    # non-ASCII character and rasters much better than 'pdf'.
    return(ggplot2::ggsave(fname, plot, device=device, ...))

#' Construct scaled polygons from segments.
#'
#' Endpoints of consecutive segments are moved to their intersection
#' if reasonably applicable to achieve a smoothly continuous form.
#' @param segments a data frame of segments.
#' @param value a vector of values to scale widths by.
#' @param max.width maximum segment width in coordinate units.
#' @param max.at value at which width is scaled to \code{max.width}.
#' @param tol segment endpoint moving tolerance relative to length.
#' @param fun function to apply to coordinates (e.g. \code{\link{round}}).
#' @return A data frame of polygons.
#' @method scale.segments not.an.s3.method
#' @export scale.segments
scale.segments = function(segments, value, max.width,
                          max.at=max(value, na.rm=TRUE), tol=0, fun=identity) {

    # Calculate offset coordinates (ixp, iyp, jxp, jyp) for each
    # segment based on the segments's value and angle, checking
    # the quadrant the segment points to to get the offset sign right.
    segments$value = value
    segments$length = with(segments, eucd(ix, iy, jx, jy))
    segments$width = value/max.at * max.width
    segments = subset(segments, length > 0)
    segments = subset(segments, !is.na(width) & width > 0)
    segments$angle = with(segments, atan(abs(jx-ix) / abs(jy-iy)))
    pdx = with(segments, ifelse(jy >= iy, 1, -1) * width * cos(angle))
    pdy = with(segments, ifelse(jx <= ix, 1, -1) * width * sin(angle))
    segments[,c("ixp", "jxp")] = segments[,c("ix", "jx")] + pdx
    segments[,c("iyp", "jyp")] = segments[,c("iy", "jy")] + pdy
    if (is.numeric(tol) && tol > 0) {
        # For each segment, find the directly following segments
        # and, of those, choose the one with the most similar
        # value to attempt an intersection with.
        time.start = Sys.time()
        messagef("Intersecting %d segment polygons...", nrow(segments))
        points = mclapply.stop(rows.along(segments), function(i) {
            if (progress.due(3, parallel=TRUE))
                progress.eta(time.start, i, nrow(segments))
            m = with(segments, which(
                ix == jx[i] & iy == jy[i] & jx != ix[i] & jy != iy[i]))
            dist = abs(value[m] - value[i])
            if (!any(is.finite(dist))) return(NULL)
            m = m[which.min(dist)]
            int = with(segments, lineint(
                ixp[i], iyp[i], jxp[i], jyp[i],
                ixp[m], iyp[m], jxp[m], jyp[m]))
            if (!is.finite(int$x)) return(NULL)
            if (!is.finite(int$y)) return(NULL)
            idist = eucd(segments$jxp[i], segments$jyp[i], int$x, int$y)
            mdist = eucd(segments$ixp[m], segments$iyp[m], int$x, int$y)
            if (idist < tol * segments$length[i] &&
                mdist < tol * segments$length[m])
                return(cbind(i, m, int$x, int$y))
            return(NULL)
        })
        points = do.call(rbind, points)
        progress.final(time.start)
        segments$jxp[points[,1]] = segments$ixp[points[,2]] = points[,3]
        segments$jyp[points[,1]] = segments$iyp[points[,2]] = points[,4]
    }
    # Create individual polygons while preserving eid-numbering.
    messagef("Creating %d polygons...", nrow(segments))
    if ("eid" %nin% colnames(segments))
        segments$eid = rows.along(segments)
    eid = rep(segments$eid, each=6)
    sid = rep(rows.along(segments), each=6)
    polys = data.frame(eid=eid, sid=sid, x=NA, y=NA)
    polys$x[seq(1, nrow(polys), 6)] = segments$ix
    polys$x[seq(2, nrow(polys), 6)] = segments$jx
    polys$x[seq(3, nrow(polys), 6)] = segments$jxp
    polys$x[seq(4, nrow(polys), 6)] = segments$ixp
    polys$x[seq(5, nrow(polys), 6)] = segments$ix
    polys$x[seq(6, nrow(polys), 6)] = NA
    polys$y[seq(1, nrow(polys), 6)] = segments$iy
    polys$y[seq(2, nrow(polys), 6)] = segments$jy
    polys$y[seq(3, nrow(polys), 6)] = segments$jyp
    polys$y[seq(4, nrow(polys), 6)] = segments$iyp
    polys$y[seq(5, nrow(polys), 6)] = segments$iy
    polys$y[seq(6, nrow(polys), 6)] = NA
    polys$x = fun(polys$x)
    polys$y = fun(polys$y)
    return(downclass(polys))
}

#' Labels for scaled segments.
#' @param segments a data frame of segments.
#' @param polys a data frame of segment polygons as returned
#' by \code{\link{scale.segments}}.
#' @param vjust.above vjust specification for labels above, logically <= 0.
#' @param vjust.below vjust specification for labels below, logically >= 1.
#' @return A data frame of labels.
#' @export segment.labels
segment.labels = function(segments, polys, vjust.above=-0.1, vjust.below=1.1) {
    labels = as.data.frame(segments)
    points = fold(polys, .(eid), x=mean(x[3:4]), y=mean(y[3:4]))
    labels = unpick(labels, x, y)
    labels = leftjoin(labels, points, by="eid")
    labels$length = with(labels, eucd(ix, iy, jx, jy))
    labels$angle = with(labels, rad2deg(atan2(jy-iy, jx-ix)))
    above = labels$angle >= -90 & labels$angle <= 90
    labels$angle[!above] = labels$angle[!above] + 180
    labels$vjust = ifelse(above, vjust.below, vjust.above)
    return(labels)
}

#' Polygon staircase legend for scaled segments.
#' @param x0 X-coordinate of point to start legend from.
#' @param y0 Y-coordinate of point to start legend from.
#' @param values a vector of values at breaks. If sorted ascending, legend will
#' also ascend from \code{(x0,y0)}, if desceding, legend will also descend.
#' To acquire these from data, see e.g. \code{\link{pretty}}.
#' @param width width of the widest segment. If using
#' \code{\link{scale.segments}}, set this to the same as \code{max.width}
#' passed there. If \code{width} is positive, construct legend east of
#' \code{x0}, if negative, west of \code{x0}.
#' @param height height of one segment. If height is positive, construct
#' legend north of \code{y0}, if negative, south of \code{y0}.
#' @param xoffset amount of space to leave between polygon and associated
#' labels. This would depend on the amount of characters in \code{values}
#' and font size and horizontal justification used when plotting.
#' @return A list of two data frames: "polys" and "labels".
#' @export segment.legend
segment.legend = function(x0, y0, values, width, height, xoffset) {
    polys = lapply(seq_along(values), function(i) {
        x = c(x0, x0 + (values[i]/max(values)) * width)
        y = c(y0 + (i-1)*height, y0 + i*height)
        x = c(min(x), min(x), max(x), max(x), min(x), NA)
        y = c(min(y), max(y), max(y), min(y), min(y), NA)
        return(data.frame(eid=i, sid=1L, x=x, y=y))
    })
    polys = rbind_all(polys)
    xmax = max(polys$x, na.rm=TRUE)
    xmin = min(polys$x, na.rm=TRUE)
    labels = lapply(uind(polys$eid), function(ii) {
        x = ifelse(width > 0, xmax + xoffset, xmin - xoffset)
        y = mean(c(min(polys$y[ii], na.rm=TRUE),
                   max(polys$y[ii], na.rm=TRUE)))

        label = values[polys$eid[ii][1]]
        return(data.frame(x=x, y=y, label=label))
    })
    labels = rbind_all(labels)
    return(list(polys=polys, labels=labels))
}
