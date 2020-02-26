# -*- coding: us-ascii-unix -*-

#' Read points, lines or polygons from a MapInfo TAB file.
#' @param fname name of MapInfo TAB file.
#' @param stringsAsFactors should character vector be converted to factors?
#' @param encoding character encoding used in code{fname}.
#' @param ... passed to \code{\link[rgdal]{readOGR}}.
#' @return A \code{Spatial*DataFrame} object.
#' @export read.mapinfo
read.mapinfo = function(fname, stringsAsFactors=FALSE, encoding="CP1252", ...) {
    shapes = rgdal::readOGR(fname,
                            layer=rgdal::ogrListLayers(fname)[1],
                            stringsAsFactors=FALSE,
                            encoding="",
                            use_iconv=FALSE,
                            ...)

    # GDAL's encoding handling varies by platform,
    # let's do the encoding conversion ourselves.
    shapes@data = recode(shapes@data, encoding, "UTF-8")
    if (stringsAsFactors) {
        for (i in which(sapply(shapes@data, is.character)))
            shapes@data[,i] = factor(shapes@data[,i])
    }
    return(shapes)
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

#' Write points, lines or polygons to a MapInfo TAB file.
#'
#' Requires \code{ogr2ogr} in \code{$PATH}.
#' @param shapes a \code{Spatial*DataFrame} object.
#' @param fname name of MapInfo TAB file to write.
#' @param epsg projection code, see \code{\link{list.projections}}.
#' @export write.mapinfo
write.mapinfo = function(shapes, fname, epsg) {
    replname = function(extension, fname="")
        gsub("\\.TAB$", extension, fname, ignore.case=TRUE)
    stopifnot(grepl("\\.TAB$", fname))
    if (is.na(proj.ogr.mapinfo(epsg)))
        stop(sprintf("EPSG %s not supported", epsg))
    shapes@data = recode(shapes@data, NULL, "CP1252")
    messagef("Writing '%s'...", fname)
    # Due to writeOGR failing to overwrite files,
    # write to tempdir and copy on from there.
    tempname = tempfile(fileext=".TAB")
    projname = replname(".PRJ", tempname)
    extensions = c(".DAT", ".ID", ".MAP", ".TAB")
    upper = sapply(toupper(extensions), replname, tempname)
    lower = sapply(tolower(extensions), replname, tempname)
    tempnames = c(upper, lower, projname)
    layer = gsub("\\..*$", "", basename(fname))
    unlink(tempnames)
    rgdal::writeOGR(shapes, tempname, layer=layer, driver="MapInfo File")
    if (!file.exists(tempname)) {
        # GDAL forces lower- and uppercase filename
        # extensions inconsistently across platforms.
        tempname = replname(".tab", tempname)
        stopifnot(file.exists(tempname))
    }
    tempname2 = replname("-2.TAB", tempname)
    tempnames = c(tempnames, sapply(toupper(extensions), replname, tempname2))
    tempnames = c(tempnames, sapply(tolower(extensions), replname, tempname2))
    # Since Proj.4 definitions are often bad,
    # overwrite the projection info with our own definition.
    cat(proj.ogr.mapinfo(epsg), file=projname)
    Sys.sleep(1)
    system(sexpf('ogr2ogr -f "MapInfo File"',
                 "-a_srs {shQuote(projname)}",
                 "{shQuote(tempname2)}",
                 "{shQuote(tempname)}"))

    for (extension in extensions) {
        src = replname(extension, tempname2)
        if (!file.exists(src)) {
            # GDAL forces lower- and uppercase filename
            # extensions inconsistently across platforms.
            src = replname(tolower(extension), tempname2)
            stopifnot(file.exists(src))
        }
        dst = replname(extension, fname)
        file.copy(src, dst, overwrite=TRUE)
    }
    Sys.sleep(1)
    unlink(tempnames)
    return(invisible())
}

#' Write points, lines or polygons to a MapInfo MIF file.
#' @param shapes a \code{Spatial*DataFrame} object.
#' @param fname name of the MapInfo MIF file to write.
#' @param epsg projection code, see \code{\link{list.projections}}.
#' @export write.mif
write.mif = function(shapes, fname, epsg) {
    stopifnot(grepl("\\.MIF$", fname))
    if (is.na(proj.mapinfo(epsg)))
        stop(sprintf("EPSG %s not supported", epsg))
    # Character encoding needs to match the 'Charset'
    # written to the MIF-file. CP1252 is a good default.
    shapes@data = recode(shapes@data, NULL, "CP1252")
    sp::proj4string(shapes) = sp::CRS(proj.proj4(epsg))
    messagef("Writing '%s'...", fname)
    # Due to writeOGR failing to overwrite files,
    # write to tempdir and copy on from there.
    tempmif = tempfile(fileext=".MIF")
    tempmid = gsub("\\.MIF$", ".MID", tempmif)
    layer = gsub("\\..*$", "", basename(fname))
    unlink(c(tempmif, tempmid))
    rgdal::writeOGR(shapes,
                    tempmif,
                    layer=layer,
                    driver="MapInfo File",
                    dataset_options="FORMAT=MIF")
    
    # GDAL forces lower- and uppercase filename
    # extensions inconsistently across platforms.
    if (file.exists(gsub("MIF$", "mif", tempmif)))
        file.rename(gsub("MIF$", "mif", tempmif), tempmif)
    if (file.exists(gsub("MID$", "mid", tempmid)))
        file.rename(gsub("MID$", "mid", tempmid), tempmid)
    # GDAL writes the data table (MID-file) and the geometry
    # in the MIF-file correctly, but not the MIF-file header.
    # Let's write a correct header and append geometry to that.
    file.copy(tempmid, gsub("\\.MIF", ".MID", fname), overwrite=TRUE)
    data = as.data.frame(shapes)
    colnames(data) = gsub("coords.x1", "x", colnames(data))
    colnames(data) = gsub("coords.x2", "y", colnames(data))
    fout = file(fname, "w", encoding="CP1252")
    catn('Version   750', file=fout)
    catn('Charset "WindowsLatin1"', file=fout)
    catn('Delimiter ","', file=fout)
    catn(proj.mapinfo(epsg), file=fout)
    catn(sprintf("Columns %d", ncol(data)), file=fout)
    for (i in seq_along(data)) {
        cat(sprintf("  %s ", colnames(data)[i]), file=fout)
        if (is.logical(data[,i])) {
            catn("Logical", file=fout)
        } else if (is.integer(data[,i])) {
            catn("Integer", file=fout)
        } else if (is.numeric(data[,i])) {
            catn("Float", file=fout)
        } else if (is.character(data[,i])) {
            # This might return incorrect results with
            # invalid multibyte strings etc.
            n = nchar(data[,i], type="width")
            n = max(n, 1, na.rm=TRUE)
            catn(sprintf("Char(%d)", n), file=fout)
        } else {
            stop(sprintf("Unrecognized column type for %s: %s",
                         colnames(data)[i],
                         class(data[,i])))

        }
    }
    catn("Data", file=fout)
    fin = file(tempmif, "r")
    in.header = TRUE
    while (TRUE) {
        if (in.header) {
            line = readLines(fin, 1)
            if (length(line) == 0) break
            in.header = !grepl("^Data$", line)
        } else {
            lines = readLines(fin, 1000000)
            if (length(lines) == 0) break
            writeLines(lines, fout)
        }
    }
    close(fin)
    close(fout)
    unlink(c(tempmif, tempmid))
    return(invisible())
}
