# -*- coding: us-ascii-unix -*-

#' This function has been removed from the package. Use `sf::st_read()` instead.
#' @param fname name of GeoJSON file.
#' @param stringsAsFactors should character vectors be converted to factors?
#' @param encoding character encoding used in code{fname}.
#' @param ... passed to \code{\link[rgdal]{readOGR}}.
#' @return A \code{Spatial*DataFrame} object.
#' @export read.geojson
read.geojson = function(fname, stringsAsFactors=FALSE, encoding="UTF-8", ...) {
    stop(
        "The function 'read.geojson' has been removed from this package."
    )
}

#' This function has been removed from the package. Use `sf::st_write()`
#' instead.
#' @param shapes a \code{Spatial*DataFrame} object.
#' @param fname name of GeoJSON file to write.
#' @param epsg projection code of input data, see
#' \code{\link{list.projections}}. Output is always WGS 84 and
#' automatically reprojected to it if needed.
#' @export write.geojson
write.geojson = function(shapes, fname, epsg) {
    stop(
        "The function 'write.geojson' has been removed from this package."
    )
}
