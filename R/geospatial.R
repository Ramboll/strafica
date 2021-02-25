#' Read points, lines or polygons from a geospatial file
#'
#' @param fname name of geospatial file
#' @return A \code{Spatial*DataFrame} object.
#' @export read_geospatial_file
read_geospatial_file = function(fname) {
    file_ext = get_file_ext(new_connection)
    if(file_ext %nin% c(".shp", ".gpkg", ".TAB")) {
        error_message = messagef("Filetype '%s' not supported.
                                 Please use '.shp', .gpkg' or '.TAB'")
        stop(error_message)
    }
    if(file_ext == ".shp") {
        geospatial_file = read.shape(new_connection)
    }
    if(file_ext == ".gpkg") {
        geospatial_file = read.gpkg(new_connection)
    }
    if(file_ext == ".TAB") {
        geospatial_file = read.mapinfo(new_connection)
    }
    return(geospatial_file)
}
