# -*- coding: us-ascii-unix -*-

#' This function has been removed from the package. Use `sf::st_read()` instead.
#' @param fname name of MapInfo TAB file.
#' @param stringsAsFactors should character vector be converted to factors?
#' @param encoding character encoding used in code{fname}.
#' @param ... passed to \code{\link[rgdal]{readOGR}}.
#' @return A \code{Spatial*DataFrame} object.
#' @export read.mapinfo
read.mapinfo = function(fname, stringsAsFactors=FALSE, encoding="CP1252", ...) {
    stop(
        "The function 'read.mapinfo' has been removed from this package."
    )
}

#' Read bounding box from MapInfo TAB file.
#' @param fname name of MapInfo TAB file.
#' @return A list with items "xmin", "xmax", "ymin" and "ymax".
#' @export read.mapinfo.bbox
read.mapinfo.bbox = function(fname) {
    lines = readLines(fname)
    lines = grep("^ *\\(-?[0-9.]+,-?[0-9.]+\\) ", lines, value=TRUE)
    x = as.numeric(gsub("^ *\\((-?[0-9.-]+),(-?[0-9.-]+)\\) .*$", "\\1", lines))
    y = as.numeric(gsub("^ *\\((-?[0-9.-]+),(-?[0-9.-]+)\\) .*$", "\\2", lines))
    return(list(xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y)))
}

#' Read and crop geographically referenced PNG-file saved by MapInfo.
#' @param fname name of PNG-file.
#' @param xmin minimum of X-coordinates to crop image to.
#' @param xmax maximum of X-coordinates to crop image to.
#' @param ymin minimum of Y-coordinates to crop image to.
#' @param ymax maximum of Y-coordinates to crop image to.
#' @param cache \code{TRUE} to cache the uncropped PNG between calls.
#' @return An image array.
#' @export read.mapinfo.png
read.mapinfo.png = local({
    pngs = list()
    function(fname, xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL, cache=FALSE) {
        if (cache && fname %nin% names(pngs))
            pngs[[fname]] <<- png::readPNG(fname)
        image = if (cache) pngs[[fname]] else png::readPNG(fname)
        fname.tab = gsub("\\.[^.]*$", ".TAB", fname)
        stopifnot(file.exists(fname.tab))
        bbox = read.mapinfo.bbox(fname.tab)
        bbox$xrange = bbox$xmax - bbox$xmin
        bbox$yrange = bbox$ymax - bbox$ymin
        xmin = if (is.numeric(xmin)) xmin else bbox$xmin
        xmax = if (is.numeric(xmax)) xmax else bbox$xmax
        ymin = if (is.numeric(ymin)) ymin else bbox$ymin
        ymax = if (is.numeric(ymax)) ymax else bbox$ymax
        stopifnot(xmin >= bbox$xmin)
        stopifnot(xmax <= bbox$xmax)
        stopifnot(ymin >= bbox$ymin)
        stopifnot(ymax <= bbox$ymax)
        xoffset.min = round(((xmin - bbox$xmin) / bbox$xrange) * ncol(image))
        xoffset.max = round(((bbox$xmax - xmax) / bbox$xrange) * ncol(image))
        yoffset.min = round(((ymin - bbox$ymin) / bbox$yrange) * nrow(image))
        yoffset.max = round(((bbox$ymax - ymax) / bbox$yrange) * nrow(image))
        xcrop = (1 + xoffset.min):(ncol(image) - xoffset.max)
        ycrop = (1 + yoffset.max):(nrow(image) - yoffset.min)
        return(image[ycrop, xcrop,])
    }
})

#' This function has been removed from the package. Use `sf::st_write()`
#' instead.
#'
#' Requires \code{ogr2ogr} in \code{$PATH}.
#' @param shapes a \code{Spatial*DataFrame} object.
#' @param fname name of MapInfo TAB file to write.
#' @param epsg projection code, see \code{\link{list.projections}}.
#' @export write.mapinfo
write.mapinfo = function(shapes, fname, epsg) {
    stop(
        "The function 'write.mapinfo' has been removed from this package."
    )
}

#' This function has been removed from the package. Use `sf::st_write()`
#' instead.
#' @param shapes a \code{Spatial*DataFrame} object.
#' @param fname name of the MapInfo MIF file to write.
#' @param epsg projection code, see \code{\link{list.projections}}.
#' @export write.mif
write.mif = function(shapes, fname, epsg) {
    stop(
        "The function 'write.mif' has been removed from this package."
    )
}
