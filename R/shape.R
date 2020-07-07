# -*- coding: us-ascii-unix -*-

#' Read points, lines or polygons from a shapefile.
#' @param fname name of shapefile.
#' @param stringsAsFactors should character vector be converted to factors?
#' @param encoding character encoding used in code{fname}.
#' Defaults to "Latin1", which is the original standard DBF encoding,
#' but in reality this will vary.
#' @param ... passed to \code{\link[rgdal]{readOGR}}.
#' @return A \code{Spatial*DataFrame} object.
#' @export read.shape
read.shape = function(fname, stringsAsFactors=FALSE, encoding="Latin1", ...) {
    shapes = rgdal::readOGR(fname,
                            layer=rgdal::ogrListLayers(fname)[1],
                            stringsAsFactors=FALSE,
                            encoding="",
                            use_iconv=FALSE,
                            integer64="allow.loss",
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

#' Write points, lines or polygons to a shapefile.
#' @param shapes a \code{Spatial*DataFrame} object.
#' @param fname name of shapefile to write.
#' @param epsg projection code, see \code{\link{list.projections}}.
#' @export write.shape
write.shape = function(shapes, fname, epsg) {
    message("Writing shapefile... Please consider geopackage instead using `write.gpkg()`.")
    if (is.na(proj.shape(epsg)))
        stop(sprintf("EPSG %s not supported", epsg))
    # Latin 1 is the original standard DBF encoding.
    shapes@data = recode(shapes@data, NULL, "Latin1")
    messagef("Writing '%s'...", fname)
    # Due to writeOGR failing to overwrite files,
    # write to tempdir and copy on from there.
    tempname = tempfile(fileext=".shp")
    extensions = c(".dbf", ".prj", ".shp", ".shx")
    tempnames = sapply(extensions, function(x) gsub("\\.shp$", x, tempname))
    layer = gsub("\\..*$", "", basename(fname))
    unlink(tempnames)
    rgdal::writeOGR(shapes, tempname, layer=layer, driver="ESRI Shapefile")
    # Since Proj.4 definitions are often bad,
    # overwrite the projection file with our own definition.
    cat(proj.shape(epsg), file=gsub("\\.shp$", ".prj", tempname))
    for (extension in extensions) {
        src = gsub("\\.shp$", extension, tempname)
        dst = gsub("\\.shp$", extension, fname)
        file.copy(src, dst, overwrite=TRUE)
        Sys.sleep(1)
        unlink(src)
    }
    return(invisible())
}
