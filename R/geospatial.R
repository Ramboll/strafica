#' Read points, lines or polygons from a geospatial file
#'
#' @param fname name of geospatial file (.shp, .gpkg or .TAB)
#' @return A \code{Spatial*DataFrame} object.
#' @export read_geospatial_file
read_geospatial_file = function(fname) {
    file_ext = get_file_ext(fname)
    if(file_ext == ".shp") {
        return(read.shape(fname))
    } else if(file_ext == ".gpkg") {
        return(read.gpkg(fname))
    } else if(file_ext == ".TAB") {
        return(read.mapinfo(fname))
    } else {
        error_message = messagef("Filetype '%s' not supported. Please use '.shp', .gpkg' or '.TAB'",
                                 file_ext)
        stop(error_message)
    }
}
