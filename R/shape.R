# -*- coding: us-ascii-unix -*-

#' This function has been removed from the package. Use `sf::st_read()` instead.
#' @param fname name of shapefile.
#' @param stringsAsFactors should character vector be converted to factors?
#' @param encoding character encoding used in code{fname}.
#' Defaults to "Latin1", which is the original standard DBF encoding,
#' but in reality this will vary.
#' @param ... passed to \code{\link[rgdal]{readOGR}}.
#' @return A \code{Spatial*DataFrame} object.
#' @export read.shape
read.shape = function(fname, stringsAsFactors=FALSE, encoding="Latin1", ...) {
    stop(
        "The function 'read.shape' has been removed from this package."
    )
}

#' This function has been removed from the package. Use `sf::st_write()`
#' instead.
#' @param shapes a \code{Spatial*DataFrame} object.
#' @param fname name of shapefile to write.
#' @param epsg projection code, see \code{\link{list.projections}}.
#' @param verbose a logical indicating whether or not to print out progress updates.
#' @export write.shape
write.shape = function(shapes, fname, epsg, verbose=TRUE) {
    stop(
        "The function 'write.shape' has been removed from this package."
    )
}
