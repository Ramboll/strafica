# -*- coding: us-ascii-unix -*-

#' Download and merge webmap tiles to form one image file.
#'
#' Requires \code{montage} from \code{imagemagick}.
#' @param template URL template with variables $X, $Y and $Z,
#' replaced with X-tile number, Y-tile number and zoom level,
#' e.g. \code{http://tile.openstreetmap.org/$Z/$X/$Y.png}.
#' @param zoom zoom level to download.
#' @param xmin WGS 84 coordinate of  west edge of area to download.
#' @param xmax WGS 84 coordinate of  east edge of area to download.
#' @param ymin WGS 84 coordinate of south edge of area to download.
#' @param ymax WGS 84 coordinate of north edge of area to download.
#' @param fname name of image file to save result to.
#' @param size pixel size of tiles (usually 256, 512 or 1024).
#' @param epsg projection code of written MapInfo file:
#' either 4326 for WGS 84 or 3857 for Web Mercator.
#' @param wait.range range of seconds from which to sample time to wait
#' between downloading successive 5x5 blocks of tiles. Using zeros
#' will skip waiting, but you will risk being banned by the tile server.
#' @export download.webmap
download.webmap = function(template, zoom, xmin, xmax, ymin, ymax, fname,
                           size=256, epsg=4326, wait.range=c(1,3)) {

    stopifnot(epsg %in% c(3857, 4326))
    bbox = data.frame(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
    bbox = transform(bbox,
                     xmin=webmap.xdeg2num(xmin, zoom),
                     xmax=webmap.xdeg2num(xmax, zoom),
                     ymin=webmap.ydeg2num(ymax, zoom),
                     ymax=webmap.ydeg2num(ymin, zoom))

    extension = utils::tail(unlist(strsplit(fname, ".", fixed=TRUE)), 1)
    dirname = file.path(tempdir(), format(Sys.time(), "webmap-%Y%m%d%H%M%S"))
    dir.create(dirname)
    tiles = expand.grid(x=bbox$xmin:bbox$xmax, y=bbox$ymin:bbox$ymax)
    # Order and download tiles in 5x5 blocks hoping to avoid being detected
    # as doing automated downloads and being banned by the server.
    tiles$order = classify(floor(tiles$x/5), floor(tiles$y/5))
    tiles = arrange(tiles, order)
    time.start = Sys.time()
    messagef("Downloading %d tiles...", nrow(tiles))
    for (i in rows.along(tiles)) {
        if (i > 1 && tiles$order[i] != tiles$order[i-1])
            # Wait a random amount of time hoping to avoid being detected
            # as doing automated downloads and being banned by the server.
            Sys.sleep(stats::runif(1, wait.range[1], wait.range[2]))
        progress.eta(time.start, i, nrow(tiles))
        x = tiles$x[i]
        y = tiles$y[i]
        url = as.character(template)
        url = gsub("\\$Z", zoom, gsub("\\$Y", y, gsub("\\$X", x, url)))
        fname.tile = sprintf("%s/%d-%d.%s", dirname, y, x, extension)
        download.file.retry(url, fname.tile, mode="wb", quiet=TRUE)
    }
    progress.final(time.start)
    if (file.exists(fname)) unlink(fname)
    messagef("Writing '%s'...", fname)
    nx = length(unique(tiles$x))
    ny = length(unique(tiles$y))
    system.message(sexpf("montage {dirname}/*.{extension}",
                         "-monitor",
                         "-limit memory 1024mb",
                         "-geometry {size}x{size}+0+0",
                         "-tile {nx}x{ny}",
                         "-background none",
                         "{fname}"))

    unlink(dirname, recursive=TRUE)
    bbox = transform(bbox,
                     xmin=webmap.xnum2deg(xmin+0, zoom),
                     xmax=webmap.xnum2deg(xmax+1, zoom),
                     ymin=webmap.ynum2deg(ymax+1, zoom),
                     ymax=webmap.ynum2deg(ymin+0, zoom))

    if (epsg == 3857) {
        bbox = reproject(bbox, 4326, 3857, c("xmin", "ymin"))
        bbox = reproject(bbox, 4326, 3857, c("xmax", "ymax"))
        projection = 'CoordSys Earth Projection 10, 157, "m", 0'
        units = 'Units "m"'
    } else {
        projection = 'CoordSys Earth Projection 1, 104'
        units = 'Units "degree"'
    }
    fname.tab = gsub(sprintf("%s$", extension), "TAB", fname)
    messagef("Writing '%s'...", fname.tab)
    f = file(fname.tab, "w", encoding="CP1252")
    writeLines(sexpf(
        '!table',
        '!version 400',
        '!charset WindowsLatin1',
        '',
        'Definition Table',
        '  File "{basename(fname)}"',
        '  Type "RASTER"',
        '  ({bbox$xmin:.6f},{bbox$ymax:.6f}) (0,0) Label "Pt 1",',
        '  ({bbox$xmax:.6f},{bbox$ymax:.6f}) ({nx*size:d},0) Label "Pt 2",',
        '  ({bbox$xmax:.6f},{bbox$ymin:.6f}) ({nx*size:d},{ny*size:d}) Label "Pt 3"',
        '  {projection}',
        '  {units}',
        sep="\n"), f)

    close(f)
    return(invisible())
}

#' Convert webmap WGS 84 X-coordinate to X-tile number.
#' @param x WGS 84 X-coordinate.
#' @param zoom zoom level.
#' @return X-tile number as integer.
#' @references \url{http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}
#' @export webmap.xdeg2num
webmap.xdeg2num = function(x, zoom) {
    xmerc = deg2rad(x)
    xtile = (1 + xmerc/pi) / 2 * 2^zoom
    return(as.integer(floor(xtile)))
}

#' Convert webmap WGS 84 Y-coordinate to Y-tile number.
#' @param y WGS 84 Y-coordinate.
#' @param zoom zoom level.
#' @return Y-tile number as integer.
#' @references \url{http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}
#' @export webmap.ydeg2num
webmap.ydeg2num = function(y, zoom) {
    ymerc = asinh(tan(deg2rad(y)))
    ytile = (1 - ymerc/pi) / 2 * 2^zoom
    return(as.integer(floor(ytile)))
}

#' Convert webmap X-tile number to WGS 84 X-coordinate.
#' @param xtile X-tile number.
#' @param zoom zoom level.
#' @return WGS 84 X-coordinate as numeric.
#' @references \url{http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}
#' @export webmap.xnum2deg
webmap.xnum2deg = function(xtile, zoom) {
    xmerc = (xtile / 2^zoom * 2 - 1) * pi
    return(rad2deg(xmerc))
}

#' Convert webmap Y-tile number to WGS 84 Y-coordinate.
#' @param ytile Y-tile number.
#' @param zoom zoom level.
#' @return WGS 84 Y-coordinate as numeric.
#' @references \url{http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}
#' @export webmap.ynum2deg
webmap.ynum2deg = function(ytile, zoom) {
    ymerc = (1 - ytile / 2^zoom * 2) * pi
    return(rad2deg(atan(sinh(ymerc))))
}
