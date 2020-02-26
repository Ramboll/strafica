# -*- coding: us-ascii-unix -*-

#' Read points, lines or polygons from a GeoPackage file
#' @param fname name of GeoPackage file
#' @param stringsAsFactors should character vector be converted to factors?
#' @param encoding character encoding used in code{fname}.
#' @param ... passed to \code{\link[rgdal]{readOGR}}.
#' @return A \code{Spatial*DataFrame} object.
#' @export read.gpkg
read.gpkg = function(fname, stringsAsFactors=FALSE, encoding="UTF-8", ...) {
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

#' Write points, lines or polygons to a GeoPackage file.
#' @param shapes a \code{Spatial*DataFrame} object.
#' @param fname name of GeoPackage file to write.
#' @param epsg projection code, see \code{\link{list.projections}}.
#' @export write.gpkg
write.gpkg = function(shapes, fname, epsg) {
    shapes@data = recode(shapes@data, NULL, "UTF-8")
    messagef("Writing '%s'...", fname)
    # Due to writeOGR failing to overwrite files,
    # write to tempdir and copy on from there.
    tempname = tempfile(fileext=".gpkg")
    layer = gsub("\\..*$", "", basename(fname))
    unlink(tempname)
    rgdal::writeOGR(shapes, tempname, layer=layer, driver="GPKG")
    file.copy(tempname, fname, overwrite=TRUE)
    Sys.sleep(1)
    unlink(tempname)
    return(invisible())
}
