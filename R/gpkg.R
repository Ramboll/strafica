# -*- coding: us-ascii-unix -*-

#' This function has been removed from the package. Use `sf::st_read()` instead.
#' @param fname name of GeoPackage file
#' @param stringsAsFactors should character vector be converted to factors?
#' @param encoding character encoding used in code{fname}.
#' @param ... passed to \code{\link[rgdal]{readOGR}}.
#' @return A \code{Spatial*DataFrame} object.
#' @export read.gpkg
read.gpkg = function(fname, stringsAsFactors=FALSE, encoding="UTF-8", ...) {
    stop(
        "The function 'read.gpkg' has been removed from this package."
    )
}


#' This function has been removed from the package. Use `sf::st_write()`
#' instead.
#' @param shapes a \code{Spatial*DataFrame} object.
#' @param fname name of GeoPackage file to write.
#' @param epsg projection code, see \code{\link{list.projections}}.
#' @export write.gpkg
write.gpkg = function(shapes, fname, epsg = NULL) {
    stop(
        "The function 'write.gpkg' has been removed from this package."
    )
}
