# -*- coding: us-ascii-unix -*-

#' Add standard tile sources and selector to map.
#' @param map A HTML widget object from \code{\link[leaflet]{leaflet}} function.
.add.standard.tiles = function(map) {
    tiles = list(
        c("Google", "http://mts.google.com/vt/lyrs=m&x={x}&y={y}&z={z}", "\u00a9 Google"),
        c("HERE Grey", "http://maps.nlp.nokia.com/maptile/2.1/maptile/newest/normal.day.grey/{z}/{x}/{y}/256/png8", "\u00a9 HERE"),
        c("HERE Satellite", "http://maps.nlp.nokia.com/maptile/2.1/maptile/newest/satellite.day/{z}/{x}/{y}/256/png8", "\u00a9 HERE"),
        c("OpenStreetMap", "http://tile.openstreetmap.org/{z}/{x}/{y}.png", "\u00a9 OpenStreetMap contributors"),
        c("OpenCycleMap", "http://a.tile.thunderforest.com/cycle/{z}/{x}/{y}.png", "\u00a9 Thunderforest, OpenStreetMap contributors"),
        c("Thunderforest Transport", "http://a.tile.thunderforest.com/transport/{z}/{x}/{y}.png", "\u00a9 Thunderforest, OpenStreetMap contributors"),
        c("CartoDB Light", "http://1.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png", "\u00a9 CartoDB, OpenStreetMap contributors")
    )
    for (tile in tiles)
        map = leaflet::addTiles(map=map,
                                urlTemplate=tile[2],
                                attribution=tile[3],
                                group=tile[1])
    
    groups = sapply(tiles, function(x) x[1])
    return(leaflet::addLayersControl(map, baseGroups=groups))
}

#' View lines on an interactive web map.
#' @param lines a data frame of lines.
#' @param data a data frame of per-entry attributes.
#' @param epsg projection code of input data, see
#' \code{\link{list.projections}}.
#' @return A map object. (Usually you don't need this,
#' just call the function without assignment.)
#' @export view.lines
view.lines = function(lines, data=NULL, epsg=4326) {
    lines = transform(lines, lng=x, lat=y)
    lines = reproject(lines, epsg, 4326, c("lng", "lat"))
    if (is.null(data))
        data = data.frame(eid=unique(lines$eid))
    # It seems popup should be one row per object, which probably
    # means geometry and data need to be in the same order.
    lines = arrange(lines, eid, sid)
    data = arrange(data, eid)
    labels = sprintf("<b>%s</b>", colnames(data))
    data$popup = sapply(rows.along(data), function(i) {
        popup = paste(labels, data[i,], sep=": ")
        return(paste(popup, collapse="<br>"))
    })
    data$popup = sprintf("<code>%s</code>", data$popup)
    data$popup = recode(data$popup, "default", "UTF-8")
    map = leaflet::leaflet(lines)
    map = .add.standard.tiles(map)
    return(leaflet::addPolylines(map,
                                 lng=~lng,
                                 lat=~lat,
                                 popup=data$popup,
                                 color="#0540ff",
                                 weight=5,
                                 opacity=0.4,
                                 smoothFactor=0))
    
}

#' View points on an interactive web map.
#' @param points a data frame of points.
#' @param epsg projection code of input data, see
#' \code{\link{list.projections}}.
#' @return A map object. (Usually you don't need this,
#' just call the function without assignment.)
#' @export view.points
view.points = function(points, epsg=4326) {
    points = transform(points, lng=x, lat=y)
    points = reproject(points, epsg, 4326, c("lng", "lat"))
    labels = sprintf("<b>%s</b>", colnames(points))
    points$popup = sapply(rows.along(points), function(i) {
        popup = paste(labels, points[i,], sep=": ")
        return(paste(popup, collapse="<br>"))
    })
    points$popup = sprintf("<code>%s</code>", points$popup)
    points$popup = recode(points$popup, "default", "UTF-8")
    map = leaflet::leaflet(points)
    map = .add.standard.tiles(map)
    return(leaflet::addMarkers(map,
                               lng=~lng,
                               lat=~lat,
                               popup=~popup))
    
}

#' View polygons on an interactive web map.
#' @param polys a data frame of polygons.
#' @param data a data frame of per-entry attributes.
#' @param epsg projection code of input data, see
#' \code{\link{list.projections}}.
#' @return A map object. (Usually you don't need this,
#' just call the function without assignment.)
#' @export view.polys
view.polys = function(polys, data=NULL, epsg=4326) {
    polys = transform(polys, lng=x, lat=y)
    polys = reproject(polys, epsg, 4326, c("lng", "lat"))
    if (is.null(data))
        data = data.frame(eid=unique(polys$eid))
    # It seems popup should be one row per object, which probably
    # means geometry and data need to be in the same order.
    polys = arrange(polys, eid, sid)
    data = arrange(data, eid)
    labels = sprintf("<b>%s</b>", colnames(data))
    data$popup = sapply(rows.along(data), function(i) {
        popup = paste(labels, data[i,], sep=": ")
        return(paste(popup, collapse="<br>"))
    })
    data$popup = sprintf("<code>%s</code>", data$popup)
    data$popup = recode(data$popup, "default", "UTF-8")
    map = leaflet::leaflet(polys)
    map = .add.standard.tiles(map)
    return(leaflet::addPolygons(map,
                                lng=~lng,
                                lat=~lat,
                                popup=data$popup,
                                stroke=TRUE,
                                color="#0540ff",
                                weight=5,
                                opacity=0.4,
                                fill=TRUE,
                                fillColor="#0540ff",
                                fillOpacity=0.1,
                                smoothFactor=0))
    
}

#' View segments on an interactive web map.
#' @param segments a data frame of segments.
#' @param epsg projection code of input data, see
#' \code{\link{list.projections}}.
#' @return A map object. (Usually you don't need this,
#' just call the function without assignment.)
#' @export view.segments
view.segments = function(segments, epsg=4326) {
    if (!exists("eid", segments))
        segments$eid = rows.along(segments)
    if (!exists("sid", segments))
        segments$sid = rows.along(segments)
    lines = segments.to.lines(segments)
    data = subset(lines, !duplicated(eid))
    data = unpick(data, x, y)
    lines = pick(lines, eid, sid, x, y)
    return(view.lines(lines, data, epsg))
}

#' View points on an interactive web map.
#' @param points a data frame of points.
#' @param token Mapbox access token.
#' @param epsg projection code of input data, see
#' \code{\link{list.projections}}.
#' @return A map object. (Usually you don't need this,
#' just call the function without assignment.)
#' @export view.points.mapbox
view.points.mapbox = function(points, token, epsg=4326){
    Sys.setenv('MAPBOX_TOKEN' = token)
    points = transform(points, lon=x, lat=y)
    points = reproject(points, epsg, 4326, c("lon", "lat"))
    zoom = ggmap::calc_zoom(lon, lat, points)
    labels = sprintf("<b>%s</b>", colnames(points))
    points$popup = sapply(rows.along(points), function(i) {
        popup = paste(labels, points[i,], sep=": ")
        return(paste(popup, collapse="<br>"))
    })
    points$popup = recode(points$popup, from="default", to="UTF-8")
    return(plotly::plot_mapbox(data = points, 
                               lat = ~lat, 
                               lon = ~lon,
                               mode = 'scattermapbox',
                               hovertext = ~popup) %>%
               plotly::layout(mapbox = list(style = "light",
                                            zoom = zoom - zoom/4,
                                            center = list(lat = (min(points$lat, na.rm = TRUE) + max(points$lat, na.rm = TRUE)) / 2,
                                                          lon = (min(points$lon, na.rm = TRUE) + max(points$lon, na.rm = TRUE)) / 2))))
}

#' View lines on an interactive web map.
#' @param lines a data frame of lines.
#' @param data a data frame of per-entry attributes.
#' @param token Mapbox access token.
#' @param epsg projection code of input data, see
#' \code{\link{list.projections}}.
#' @return A map object. (Usually you don't need this,
#' just call the function without assignment.)
#' @export view.lines.mapbox
view.lines.mapbox = function(lines, data=NULL, token, epsg=4326){
    Sys.setenv('MAPBOX_TOKEN' = token)
    lines = transform(lines, lon=x, lat=y)
    lines = reproject(lines, epsg, 4326, c("lon", "lat"))
    if (is.null(data))
        data = data.frame(eid=unique(lines$eid))
    labels = sprintf("<b>%s</b>", colnames(data))
    data$popup = sapply(rows.along(data), function(i) {
        popup = paste(labels, data[i,], sep=": ")
        return(paste(popup, collapse="<br>"))
    })
    data$popup = recode(data$popup, "default", "UTF-8") 
    zoom = ggmap::calc_zoom(lon, lat, lines)
    lines = leftjoin(lines, pick(data, eid, popup))
    return(plotly::plot_mapbox(mode = 'lines') %>%
               plotly::add_paths(data = dplyr::group_by(lines, eid),
                                 hovertext = ~popup,
                                 x = ~lon,
                                 y = ~lat)) %>%
        plotly::layout(mapbox = list(style = "light",
                                     zoom = zoom - zoom/5,
                                     center = list(lat = (min(lines$lat, na.rm = TRUE) + max(lines$lat, na.rm = TRUE)) / 2,
                                                   lon = (min(lines$lon, na.rm = TRUE) + max(lines$lon, na.rm = TRUE)) / 2)
        ))
}

#' View segments on an interactive web map.
#' @param segments a data frame of segments.
#' @param token Mapbox access token.
#' @param epsg projection code of input data, see
#' \code{\link{list.projections}}.
#' @return A map object. (Usually you don't need this,
#' just call the function without assignment.)
#' @export view.segments.mapbox
view.segments.mapbox = function(segments, token, epsg=4326) {
    if (!exists("eid", segments))
        segments$eid = rows.along(segments)
    if (!exists("sid", segments))
        segments$sid = rows.along(segments)
    lines = segments.to.lines(segments)
    data = subset(lines, !duplicated(eid))
    data = unpick(data, x, y)
    lines = pick(lines, eid, sid, x, y)
    return(view.lines.mapbox(lines, data, token, epsg))
}

#' View polygons on an interactive web map.
#' @param polys a data frame of polygons.
#' @param data a data frame of per-entry attributes.
#' @param token Mapbox access token.
#' @param epsg projection code of input data, see
#' \code{\link{list.projections}}.
#' @return A map object. (Usually you don't need this,
#' just call the function without assignment.)
#' @export view.polys.mapbox
view.polys.mapbox = function(polys, data=NULL, token, epsg=4326){
    Sys.setenv('MAPBOX_TOKEN' = token)
    polys = transform(polys, lon=x, lat=y)
    polys = reproject(polys, epsg, 4326, c("lon", "lat"))
    if (is.null(data))
        data = data.frame(eid=unique(polys$eid))
    labels = sprintf("<b>%s</b>", colnames(data))
    data$popup = sapply(rows.along(data), function(i) {
        popup = paste(labels, data[i,], sep=": ")
        return(paste(popup, collapse="<br>"))
    })
    data$popup = recode(data$popup, "default", "UTF-8")
    data = reproject(data, epsg, 4326)
    zoom = ggmap::calc_zoom(lon, lat, polys)
    return(plotly::plot_mapbox(data = polys,
                               # hovertext = ~popup,
                               hoverinfo = "none",
                               x = ~lon,
                               y = ~lat) %>%
               plotly::add_polygons() %>%
               plotly::add_markers(data = data,
                                   hovertext = ~popup,
                                   x = ~x,
                                   y = ~y,
                                   inherit = FALSE,
                                   color = I("white")) %>%
               plotly::layout(mapbox = list(style = "light",
                                            zoom = zoom - zoom/5,
                                            center = list(lat = (min(polys$lat, na.rm = TRUE) + max(polys$lat, na.rm = TRUE)) / 2,
                                                          lon = (min(polys$lon, na.rm = TRUE) + max(polys$lon, na.rm = TRUE)) / 2)
               )))
    
}
