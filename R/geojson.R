# -*- coding: us-ascii-unix -*-

#' Read points, lines or polygons from a GeoJSON file.
#' @param fname name of GeoJSON file.
#' @param stringsAsFactors should character vectors be converted to factors?
#' @param encoding character encoding used in code{fname}.
#' @param ... passed to \code{\link[rgdal]{readOGR}}.
#' @return A \code{Spatial*DataFrame} object.
#' @export read.geojson
read.geojson = function(fname, stringsAsFactors=FALSE, encoding="UTF-8", ...) {
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

#' Write points, lines or polygons to a GeoJSON file.
#' @param shapes a \code{Spatial*DataFrame} object.
#' @param fname name of GeoJSON file to write.
#' @param epsg projection code of input data, see
#' \code{\link{list.projections}}. Output is always WGS 84 and
#' automatically reprojected to it if needed.
#' @export write.geojson
write.geojson = function(shapes, fname, epsg) {
    # UTF-8 is the default JSON encoding.
    # http://www.ietf.org/rfc/rfc4627.txt
    shapes@data = recode(shapes@data, NULL, "UTF-8")
    if (epsg != 4326) {
        # WGS 84 is the default coordinate system for GeoJSON.
        # http://www.geojson.org/geojson-spec.html#coordinate-reference-system-objects
        message("Reprojecting to WGS 84...")
        shapes = reproject(shapes, epsg, 4326)
    }
    messagef("Writing '%s'...", fname)
    # Due to writeOGR failing to overwrite files,
    # write to tempdir and copy on from there.
    tempname = tempfile(fileext=".geojson")
    layer = gsub("\\..*$", "", basename(fname))
    unlink(tempname)
    rgdal::writeOGR(shapes, tempname, layer=layer, driver="GeoJSON")
    file.copy(tempname, fname, overwrite=TRUE)
    Sys.sleep(1)
    unlink(tempname)
    return(invisible())
}
