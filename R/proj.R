<<<<<<< HEAD
# -*- coding: us-ascii-unix -*-

# "The OGP Geodesy Subcommittee has reserved the integer range 0 to 32767 for use
# as codes. [...] To prevent conflict with future additions to the EPSG dataset,
# users who wish to augment the data with their own information should utilise
# codes greater than 32768."
# -- http://info.ogp.org.uk/geodesy/CurrentDB.html

#' Supported projections.
#' @return A data frame of EPSG codes and names.
#' @export list.projections
list.projections = function() {
    epsg = c(2056, 2391, 2392, 2393, 3008, 3067, 3776, 3857, 3877, 3878, 3879, 3880, 4258, 4326, 4839, 28992, 31467, 32632, 50001, 99999)
    name = sapply(epsg, proj.name)
    data.frame(name=name, epsg=epsg, row.names=seq_along(epsg))
}

#' MapInfo-strings of supported projections.
#' @param epsg projection EPSG code.
#' @return A string or \code{NA}.
#' @method proj.mapinfo not.an.s3.method
#' @export proj.mapinfo
proj.mapinfo = function(epsg)
    # To find out MapInfo projection strings, export a MapInfo table
    # in the MapInfo Interchange format (MIF).
    c('2056'='CoordSys Earth Projection 25, 158, "m", 7.439583333333, 46.952405555556, 2600000, 1200000 Bounds (-97400000, -98800000) (102600000, 101200000)',
      '2391'='CoordSys Earth Projection 24, 1016, "m", 21, 0, 1, 1500000, 0 Bounds (1000000, 6000000) (2500000, 8000000)',
      '2392'='CoordSys Earth Projection 24, 1016, "m", 24, 0, 1, 2500000, 0 Bounds (2000000, 6000000) (3000000, 8000000)',
      '2393'='CoordSys Earth Projection 24, 1016, "m", 27, 0, 1, 3500000, 0 Bounds (2500000, 6000000) (4500000, 8000000)',
      '3067'='CoordSys Earth Projection 8, 115, "m", 27, 0, 0.9996, 500000, 0 Bounds (-100000, 6000000) (1000000, 8000000)',
      '3776'='CoordSys Earth Projection 8, 74, "m", -114, 0, 0.9999, 0, 0',
      '3857'='CoordSys Earth Projection 10, 157, "m", 0 Bounds (-40075016.6856, -20037508.343) (40075016.6856, 20037508.343)',
      '3877'='CoordSys Earth Projection 8, 115, "m", 23, 0, 1, 23500000, 0 Bounds (23000000, 6500000) (24000000, 8000000)',
      '3878'='CoordSys Earth Projection 8, 115, "m", 24, 0, 1, 24500000, 0 Bounds (24000000, 6500000) (25000000, 8000000)',
      '3879'='CoordSys Earth Projection 8, 115, "m", 25, 0, 1, 25500000, 0 Bounds (25000000, 6500000) (26000000, 8000000)',
      '3880'='CoordSys Earth Projection 8, 115, "m", 26, 0, 1, 26500000, 0 Bounds (26000000, 6500000) (27000000, 8000000)',
      '4258'='CoordSys Earth Projection 1, 115',
      '4326'='CoordSys Earth Projection 1, 104',
      '28992'='CoordSys Earth Projection 20, 109, "m", 5.387638889, 52.156160556, 0.9999079, 155000, 463000 Bounds (-99845000, -99537000) (100155000, 100463000)',
      '50001'='CoordSys Earth Projection 24, 28, "m", 27, 0, 1, 3500000, 0 Bounds (-4923531.59409, -10002288.299) (11923531.5941, 10002288.299)',
      '99999'=''
      )[as.character(epsg)]

#' Names of supported projections.
#' @param epsg projection EPSG code.
#' @return A string or \code{NA}.
#' @method proj.name not.an.s3.method
#' @export proj.name
proj.name = function(epsg)
    c('2056'='CH1903+ / LV95',
      '2391'='KKJ1',
      '2392'='KKJ2',
      '2393'='KKJ3',
      '3008'= 'SWEREF99 12 00'
      '3067'='ETRS-TM35',
      '3776'='NAD83 / Alberta 3TM ref merid 114 W',
      '3857'='Web Mercator',
      '3877'='ETRS-GK23',
      '3878'='ETRS-GK24',
      '3879'='ETRS-GK25',
      '3880'='ETRS-GK26',
      '4258'='ETRS89',
      '4326'='WGS 84',
      '4839'='ETRS89 / LCC Germany (N-E)',
      '28992'='Amersfoort / RD New',
      '31467'='DHDN / 3-degree Gauss-Kruger zone 3',
      '32632'='WGS 84 / UTM zone 32N',
      '50001'='KKJ3 (1996)',
      '99999'='Unknown'
      )[as.character(epsg)]

#' Projection strings to write into MapInfo files using ogr2ogr.
#' @param epsg projection EPSG code.
#' @return A string or \code{NA}.
#' @method proj.ogr.mapinfo not.an.s3.method
#' @export proj.ogr.mapinfo
proj.ogr.mapinfo = function(epsg)
    # In some cases, strings from proj.shape will work fine here,
    # other cases are very mysterious.
    c('2056'='+proj=somerc +lat_0=46.952405555556 +lon_0=7.439583333333 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs',
      '2391'='PROJCS["Finland_Zone_1",GEOGCS["GCS_KKJ",DATUM["Finnish_KKJ",SPHEROID["International_1924",6378388,297],TOWGS84[-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator_MapInfo_24"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",21],PARAMETER["scale_factor",1],PARAMETER["false_easting",1500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '2392'='PROJCS["Finland_Zone_2",GEOGCS["GCS_KKJ",DATUM["Finnish_KKJ",SPHEROID["International_1924",6378388,297],TOWGS84[-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator_MapInfo_24"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",24],PARAMETER["scale_factor",1],PARAMETER["false_easting",2500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '2393'='PROJCS["Finland_Zone_3",GEOGCS["GCS_KKJ",DATUM["Finnish_KKJ",SPHEROID["International_1924",6378388,297],TOWGS84[-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator_MapInfo_24"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",27],PARAMETER["scale_factor",1],PARAMETER["false_easting",3500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3067'='PROJCS["Europe ETRS-TM35, ETRS89 datum; 24 deg East to 30 deg East",GEOGCS["ETRS89",DATUM["D_ETRS89",SPHEROID["World_Geodetic_System_of_1984_GEM_10C",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",27],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3776'=NA,
      '3857'=NA,
      '3877'='PROJCS["ETRS89_ETRS_GK23FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",23],PARAMETER["scale_factor",1],PARAMETER["false_easting",23500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3878'='PROJCS["ETRS89_ETRS_GK24FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",24],PARAMETER["scale_factor",1],PARAMETER["false_easting",24500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3879'='PROJCS["ETRS89_ETRS_GK25FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",25],PARAMETER["scale_factor",1],PARAMETER["false_easting",25500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3880'='PROJCS["ETRS89_ETRS_GK26FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",26],PARAMETER["scale_factor",1],PARAMETER["false_easting",26500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '4258'='GEOGCS["Europe ETRS 89 (EUREF 89 System), Latitude-Longitude; Degrees",DATUM["D_ETRS89",SPHEROID["World_Geodetic_System_of_1984_GEM_10C",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]',
      '4326'='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]',
      '28992'=NA,
      '50001'=NA,
      '99999'=''
      )[as.character(epsg)]

#' Proj4-strings of supported projections.
#' @param epsg projection EPSG code.
#' @return A string or \code{NA}.
#' @method proj.proj4 not.an.s3.method
#' @export proj.proj4
proj.proj4 = function(epsg) {
    # To find out Proj4 strings compatible with MapInfo,
    # see x = read.mapinfo(...); summary(x).
    out = c('2056'='+proj=somerc +lat_0=46.952405555556 +lon_0=7.439583333333 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs',
            '2391'='+proj=tmerc +lat_0=0 +lon_0=21 +k=1 +x_0=1500000 +y_0=0 +ellps=intl +towgs84=-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496 +units=m +no_defs',
            '2392'='+proj=tmerc +lat_0=0 +lon_0=24 +k=1 +x_0=2500000 +y_0=0 +ellps=intl +towgs84=-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496 +units=m +no_defs',
            '2393'='+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=3500000 +y_0=0 +ellps=intl +towgs84=-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496 +units=m +no_defs',
            '3008'='+proj=tmerc +lat_0=0 +lon_0=13.5 +k=1 +x_0=150000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
            '3067'='+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
            '3776'='+proj=tmerc +lat_0=0 +lon_0=-114 +k=0.9999 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
            '3857'='+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs',
            '3877'='+proj=tmerc +lat_0=0 +lon_0=23 +k=1 +x_0=23500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
            '3878'='+proj=tmerc +lat_0=0 +lon_0=24 +k=1 +x_0=24500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
            '3879'='+proj=tmerc +lat_0=0 +lon_0=25 +k=1 +x_0=25500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
            '3880'='+proj=tmerc +lat_0=0 +lon_0=26 +k=1 +x_0=26500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
            '4258'='+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs',
            '4326'='+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs',
            '4839'='+proj=lcc +lat_1=48.66666666666666 +lat_2=53.66666666666666 +lat_0=51 +lon_0=10.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
            '28992'='+proj=stere +lat_0=52.156160556 +lon_0=5.387638889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=593,26,478,0,0,0,0 +units=m +no_defs',
            '31467'='+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs',
            '32632'='+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs',
            '50001'='+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=3500000 +y_0=0 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs',
            '99999'=''
            )[as.character(epsg)]

    if (!is.na(out)) return(unname(out))
    out = sprintf("+init=epsg:%s", epsg)
    messagef("proj.proj4: EPSG:%s not found in strafica's database, falling back on '%s'.", epsg, out)
    return(out)
}

#' ESRI shape WKT-strings of supported projections.
#' @param epsg projection EPSG code.
#' @return A string or \code{NA}.
#' @method proj.shape not.an.s3.method
#' @export proj.shape
proj.shape = function(epsg)
    # To find out ESRI shape WKT-strings compatible with MapInfo,
    # convert from MapInfo to shape using Universal Translator.
    c('2056'='PROJCS["CH1903+_LV95",GEOGCS["GCS_CH1903+",DATUM["D_CH1903+",SPHEROID["Bessel_1841",6377397.155,299.1528128]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Hotine_Oblique_Mercator_Azimuth_Center"],PARAMETER["False_Easting",2600000.0],PARAMETER["False_Northing",1200000.0],PARAMETER["Scale_Factor",1.0],PARAMETER["Azimuth",90.0],PARAMETER["Longitude_Of_Center",7.439583333333333],PARAMETER["Latitude_Of_Center",46.95240555555556],UNIT["Meter",1.0],AUTHORITY["EPSG",2056]]',
      '2391'='PROJCS["Finland_Zone_1",GEOGCS["GCS_KKJ",DATUM["D_KKJ",SPHEROID["International_1924",6378388.0,297.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Gauss_Kruger"],PARAMETER["False_Easting",1500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",21.0],PARAMETER["Scale_Factor",1.0],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]',
      '2392'='PROJCS["Finland_Zone_2",GEOGCS["GCS_KKJ",DATUM["D_KKJ",SPHEROID["International_1924",6378388.0,297.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Gauss_Kruger"],PARAMETER["False_Easting",2500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",24.0],PARAMETER["Scale_Factor",1.0],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]',
      '2393'='PROJCS["Finland_Zone_3",GEOGCS["GCS_KKJ",DATUM["D_KKJ",SPHEROID["International_1924",6378388.0,297.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Gauss_Kruger"],PARAMETER["False_Easting",3500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",27.0],PARAMETER["Scale_Factor",1.0],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]',
      '3008'='PROJCS["SWEREF99 13 30",GEOGCS["SWEREF99",DATUM["D_SWEREF99",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",13.5],PARAMETER["scale_factor",1],PARAMETER["false_easting",150000],PARAMETER["false_northing",0],UNIT["Meter",1]]'
      '3067'='PROJCS["Europe ETRS-TM35, ETRS89 datum; 24 deg East to 30 deg East",GEOGCS["ETRS89",DATUM["D_ETRS89",SPHEROID["World_Geodetic_System_of_1984_GEM_10C",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",27],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3776'='PROJCS["3TM114-83",GEOGCS["North_American_Datum_1983",DATUM["D_North_American_1983",SPHEROID["Geodetic Reference System of 1980",6378137.0,298.2572221009113]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",-114.0],PARAMETER["scale_factor",0.9999],PARAMETER["latitude_of_origin",0.0],UNIT["Meter",1.0]]',
      '3857'='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]',
      '3877'='PROJCS["ETRS89_ETRS_GK23FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",23],PARAMETER["scale_factor",1],PARAMETER["false_easting",23500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3878'='PROJCS["ETRS89_ETRS_GK24FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",24],PARAMETER["scale_factor",1],PARAMETER["false_easting",24500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3879'='PROJCS["ETRS89_ETRS_GK25FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",25],PARAMETER["scale_factor",1],PARAMETER["false_easting",25500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3880'='PROJCS["ETRS89_ETRS_GK26FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",26],PARAMETER["scale_factor",1],PARAMETER["false_easting",26500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '4258'='GEOGCS["Europe ETRS 89 (EUREF 89 System), Latitude-Longitude; Degrees",DATUM["D_ETRS89",SPHEROID["World_Geodetic_System_of_1984_GEM_10C",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]',
      '4326'='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]',
      '4839'='PROJCS["ETRS89_LCC_Germany_N_E",GEOGCS["GCS_ETRS_1989",DATUM["D_ETRS_1989",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Lambert_Conformal_Conic"],PARAMETER["standard_parallel_1",48.66666666666666],PARAMETER["standard_parallel_2",53.66666666666666],PARAMETER["latitude_of_origin",51],PARAMETER["central_meridian",10.5],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '31467'='PROJCS["DHDN_3_degree_Gauss_Kruger_zone_3",GEOGCS["GCS_DHDN",DATUM["D_Deutsches_Hauptdreiecksnetz",SPHEROID["Bessel_1841",6377397.155,299.1528128]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",9],PARAMETER["scale_factor",1],PARAMETER["false_easting",3500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '32632'='PROJCS["WGS_1984_UTM_Zone_32N",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",9],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '28992'='PROJCS["RD_New",GEOGCS["GCS_Amersfoort",DATUM["D_Amersfoort",SPHEROID["Bessel_1841",6377397.155,299.1528128]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199432955]],PROJECTION["Double_Stereographic"],PARAMETER["False_Easting",155000],PARAMETER["False_Northing",463000],PARAMETER["Central_Meridian",5.38763888888889],PARAMETER["Scale_Factor",0.9999079],PARAMETER["Latitude_Of_Origin",52.15616055555555],UNIT["Meter",1]]',
      '50001'='PROJCS["Finnish KKJ 3 (YKJ)",GEOGCS["ERP50-FI",DATUM["D_ERP50-FI",SPHEROID["Hayford_1924_aka_1909_same_as_International_1924",6378388,297.0000000000601]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",27],PARAMETER["scale_factor",1],PARAMETER["false_easting",3500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '99999'=''
      )[as.character(epsg)]

#' Transform and convert between projections.
#' @param coords a data frame with columns "x" and "y" or
#' a two-column matrix of X- and Y-coordinates or an \pkg{sp} object.
#' @param from EPSG code of current projection.
#' @param to EPSG code of desired projection.
#' @param names names of columns holding coordinates.
#' (Only relevant if \code{coords} is a data frame.)
#' @param fun function to apply to resulting coordinates (e.g. round).
#' (Only used if \code{coords} is a data.frame or a matrix.)
#' @return A data frame or two-column matrix or an \pkg{sp} object.
#' @rdname reproject
#' @export
reproject = function(coords, from, to, names=c("x", "y"), fun=identity)
    UseMethod("reproject", coords)

#' @rdname reproject
#' @method reproject data.frame
#' @export
reproject.data.frame = function(coords, from, to, names=c("x", "y"),
                                fun=identity) {
    if (from == to)
        return(coords)
    xname = names[1]
    yname = names[2]
    # Do not convert rows where the coordinate
    # values are NA. This is typical in line
    # data where different lines are seperated
    # with a collumn where {x,y} = NA
    m = !is.na(coords[,xname]) & !is.na(coords[,yname])
    shapes = sf::st_as_sf(coords[m,], coords = names, crs = from)
    shapes = sf::st_transform(shapes, crs = to)
    shapes = sf::st_coordinates(shapes)
    coords[m,xname] = shapes[,1]
    coords[m,yname] = shapes[,2]
    coords[!m,xname] = NA
    coords[!m,yname] = NA
    return(coords)
}

#' @rdname reproject
#' @method reproject matrix
#' @export
reproject.matrix = function(coords, from, to, names=NULL, fun=identity) {
    if (from == to)
        return(coords)
    m = !is.na(coords[,1]) & !is.na(coords[,2])
    shapes = sf::st_as_sf(coords[m,], coords = 1:2, crs = from)
    shapes = sf::st_transform(shapes, crs = to)
    shapes = sf::st_coordinates(shapes)
    coords[m,1] = shapes[,1]
    coords[m,2] = shapes[,2]
    coords[!m,1] = NA
    coords[!m,2] = NA
    return(coords)
}

reproject.sp = function(coords, from, to, names=NULL, fun=NULL) {
    from = proj.proj4(from)
    to = proj.proj4(to)
    stopifnot(is.character(from))
    stopifnot(is.character(to))
    if (from == to)
        return(coords)
    sp::proj4string(coords) = sp::CRS(from)
    return(sp::spTransform(coords, sp::CRS(to)))
}

#' @rdname reproject
#' @method reproject SpatialLinesDataFrame
#' @export
reproject.SpatialLinesDataFrame = reproject.sp

#' @rdname reproject
#' @method reproject SpatialPointsDataFrame
#' @export
reproject.SpatialPointsDataFrame = reproject.sp

#' @rdname reproject
#' @method reproject SpatialPolygonsDataFrame
#' @export
reproject.SpatialPolygonsDataFrame = reproject.sp
=======
# -*- coding: us-ascii-unix -*-

# "The OGP Geodesy Subcommittee has reserved the integer range 0 to 32767 for use
# as codes. [...] To prevent conflict with future additions to the EPSG dataset,
# users who wish to augment the data with their own information should utilise
# codes greater than 32768."
# -- http://info.ogp.org.uk/geodesy/CurrentDB.html

#' Supported projections.
#' @return A data frame of EPSG codes and names.
#' @export list.projections
list.projections = function() {
    epsg = c(2056, 2391, 2392, 2393, 3008, 3067, 3776, 3857, 3877, 3878, 3879, 3880, 4258, 4326, 28992, 50001, 99999)
    name = sapply(epsg, proj.name)
    data.frame(name=name, epsg=epsg, row.names=seq_along(epsg))
}

#' MapInfo-strings of supported projections.
#' @param epsg projection EPSG code.
#' @return A string or \code{NA}.
#' @method proj.mapinfo not.an.s3.method
#' @export proj.mapinfo
proj.mapinfo = function(epsg)
    # To find out MapInfo projection strings, export a MapInfo table
    # in the MapInfo Interchange format (MIF).
    c('2056'='CoordSys Earth Projection 25, 158, "m", 7.439583333333, 46.952405555556, 2600000, 1200000 Bounds (-97400000, -98800000) (102600000, 101200000)',
      '2391'='CoordSys Earth Projection 24, 1016, "m", 21, 0, 1, 1500000, 0 Bounds (1000000, 6000000) (2500000, 8000000)',
      '2392'='CoordSys Earth Projection 24, 1016, "m", 24, 0, 1, 2500000, 0 Bounds (2000000, 6000000) (3000000, 8000000)',
      '2393'='CoordSys Earth Projection 24, 1016, "m", 27, 0, 1, 3500000, 0 Bounds (2500000, 6000000) (4500000, 8000000)',
      '3067'='CoordSys Earth Projection 8, 115, "m", 27, 0, 0.9996, 500000, 0 Bounds (-100000, 6000000) (1000000, 8000000)',
      '3776'='CoordSys Earth Projection 8, 74, "m", -114, 0, 0.9999, 0, 0',
      '3857'='CoordSys Earth Projection 10, 157, "m", 0 Bounds (-40075016.6856, -20037508.343) (40075016.6856, 20037508.343)',
      '3877'='CoordSys Earth Projection 8, 115, "m", 23, 0, 1, 23500000, 0 Bounds (23000000, 6500000) (24000000, 8000000)',
      '3878'='CoordSys Earth Projection 8, 115, "m", 24, 0, 1, 24500000, 0 Bounds (24000000, 6500000) (25000000, 8000000)',
      '3879'='CoordSys Earth Projection 8, 115, "m", 25, 0, 1, 25500000, 0 Bounds (25000000, 6500000) (26000000, 8000000)',
      '3880'='CoordSys Earth Projection 8, 115, "m", 26, 0, 1, 26500000, 0 Bounds (26000000, 6500000) (27000000, 8000000)',
      '4258'='CoordSys Earth Projection 1, 115',
      '4326'='CoordSys Earth Projection 1, 104',
      '28992'='CoordSys Earth Projection 20, 109, "m", 5.387638889, 52.156160556, 0.9999079, 155000, 463000 Bounds (-99845000, -99537000) (100155000, 100463000)',
      '50001'='CoordSys Earth Projection 24, 28, "m", 27, 0, 1, 3500000, 0 Bounds (-4923531.59409, -10002288.299) (11923531.5941, 10002288.299)',
      '99999'=''
      )[as.character(epsg)]

#' Names of supported projections.
#' @param epsg projection EPSG code.
#' @return A string or \code{NA}.
#' @method proj.name not.an.s3.method
#' @export proj.name
proj.name = function(epsg)
    c('2056'='CH1903+ / LV95',
      '2391'='KKJ1',
      '2392'='KKJ2',
      '2393'='KKJ3',
      '3008'='SWEREF99 12 00'
      '3067'='ETRS-TM35',
      '3776'='NAD83 / Alberta 3TM ref merid 114 W',
      '3857'='Web Mercator',
      '3877'='ETRS-GK23',
      '3878'='ETRS-GK24',
      '3879'='ETRS-GK25',
      '3880'='ETRS-GK26',
      '4258'='ETRS89',
      '4326'='WGS 84',
      '28992'='Amersfoort / RD New',
      '50001'='KKJ3 (1996)',
      '99999'='Unknown'
      )[as.character(epsg)]

#' Projection strings to write into MapInfo files using ogr2ogr.
#' @param epsg projection EPSG code.
#' @return A string or \code{NA}.
#' @method proj.ogr.mapinfo not.an.s3.method
#' @export proj.ogr.mapinfo
proj.ogr.mapinfo = function(epsg)
    # In some cases, strings from proj.shape will work fine here,
    # other cases are very mysterious.
    c('2056'='+proj=somerc +lat_0=46.952405555556 +lon_0=7.439583333333 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs',
      '2391'='PROJCS["Finland_Zone_1",GEOGCS["GCS_KKJ",DATUM["Finnish_KKJ",SPHEROID["International_1924",6378388,297],TOWGS84[-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator_MapInfo_24"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",21],PARAMETER["scale_factor",1],PARAMETER["false_easting",1500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '2392'='PROJCS["Finland_Zone_2",GEOGCS["GCS_KKJ",DATUM["Finnish_KKJ",SPHEROID["International_1924",6378388,297],TOWGS84[-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator_MapInfo_24"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",24],PARAMETER["scale_factor",1],PARAMETER["false_easting",2500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '2393'='PROJCS["Finland_Zone_3",GEOGCS["GCS_KKJ",DATUM["Finnish_KKJ",SPHEROID["International_1924",6378388,297],TOWGS84[-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator_MapInfo_24"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",27],PARAMETER["scale_factor",1],PARAMETER["false_easting",3500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3067'='PROJCS["Europe ETRS-TM35, ETRS89 datum; 24 deg East to 30 deg East",GEOGCS["ETRS89",DATUM["D_ETRS89",SPHEROID["World_Geodetic_System_of_1984_GEM_10C",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",27],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3776'=NA,
      '3857'=NA,
      '3877'='PROJCS["ETRS89_ETRS_GK23FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",23],PARAMETER["scale_factor",1],PARAMETER["false_easting",23500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3878'='PROJCS["ETRS89_ETRS_GK24FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",24],PARAMETER["scale_factor",1],PARAMETER["false_easting",24500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3879'='PROJCS["ETRS89_ETRS_GK25FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",25],PARAMETER["scale_factor",1],PARAMETER["false_easting",25500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3880'='PROJCS["ETRS89_ETRS_GK26FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",26],PARAMETER["scale_factor",1],PARAMETER["false_easting",26500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '4258'='GEOGCS["Europe ETRS 89 (EUREF 89 System), Latitude-Longitude; Degrees",DATUM["D_ETRS89",SPHEROID["World_Geodetic_System_of_1984_GEM_10C",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]',
      '4326'='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]',
      '28992'=NA,
      '50001'=NA,
      '99999'=''
      )[as.character(epsg)]

#' Proj4-strings of supported projections.
#' @param epsg projection EPSG code.
#' @return A string or \code{NA}.
#' @method proj.proj4 not.an.s3.method
#' @export proj.proj4
proj.proj4 = function(epsg) {
    # To find out Proj4 strings compatible with MapInfo,
    # see x = read.mapinfo(...); summary(x).
    out = c('2056'='+proj=somerc +lat_0=46.952405555556 +lon_0=7.439583333333 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs',
            '2391'='+proj=tmerc +lat_0=0 +lon_0=21 +k=1 +x_0=1500000 +y_0=0 +ellps=intl +towgs84=-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496 +units=m +no_defs',
            '2392'='+proj=tmerc +lat_0=0 +lon_0=24 +k=1 +x_0=2500000 +y_0=0 +ellps=intl +towgs84=-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496 +units=m +no_defs',
            '2393'='+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=3500000 +y_0=0 +ellps=intl +towgs84=-96.062,-82.428,-121.754,4.801,0.345,-1.376,1.496 +units=m +no_defs',
            '3008'='+proj=tmerc +lat_0=0 +lon_0=13.5 +k=1 +x_0=150000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
            '3067'='+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
            '3776'='+proj=tmerc +lat_0=0 +lon_0=-114 +k=0.9999 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
            '3857'='+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs',
            '3877'='+proj=tmerc +lat_0=0 +lon_0=23 +k=1 +x_0=23500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
            '3878'='+proj=tmerc +lat_0=0 +lon_0=24 +k=1 +x_0=24500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
            '3879'='+proj=tmerc +lat_0=0 +lon_0=25 +k=1 +x_0=25500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
            '3880'='+proj=tmerc +lat_0=0 +lon_0=26 +k=1 +x_0=26500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
            '4258'='+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs',
            '4326'='+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs',
            '28992'='+proj=stere +lat_0=52.156160556 +lon_0=5.387638889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=593,26,478,0,0,0,0 +units=m +no_defs',
            '50001'='+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=3500000 +y_0=0 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs',
            '99999'=''
            )[as.character(epsg)]

    if (!is.na(out)) return(unname(out))
    out = sprintf("+init=epsg:%s", epsg)
    messagef("proj.proj4: EPSG:%s not found in strafica's database, falling back on '%s'.", epsg, out)
    return(out)
}

#' ESRI shape WKT-strings of supported projections.
#' @param epsg projection EPSG code.
#' @return A string or \code{NA}.
#' @method proj.shape not.an.s3.method
#' @export proj.shape
proj.shape = function(epsg)
    # To find out ESRI shape WKT-strings compatible with MapInfo,
    # convert from MapInfo to shape using Universal Translator.
    c('2056'='PROJCS["CH1903+_LV95",GEOGCS["GCS_CH1903+",DATUM["D_CH1903+",SPHEROID["Bessel_1841",6377397.155,299.1528128]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Hotine_Oblique_Mercator_Azimuth_Center"],PARAMETER["False_Easting",2600000.0],PARAMETER["False_Northing",1200000.0],PARAMETER["Scale_Factor",1.0],PARAMETER["Azimuth",90.0],PARAMETER["Longitude_Of_Center",7.439583333333333],PARAMETER["Latitude_Of_Center",46.95240555555556],UNIT["Meter",1.0],AUTHORITY["EPSG",2056]]',
      '2391'='PROJCS["Finland_Zone_1",GEOGCS["GCS_KKJ",DATUM["D_KKJ",SPHEROID["International_1924",6378388.0,297.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Gauss_Kruger"],PARAMETER["False_Easting",1500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",21.0],PARAMETER["Scale_Factor",1.0],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]',
      '2392'='PROJCS["Finland_Zone_2",GEOGCS["GCS_KKJ",DATUM["D_KKJ",SPHEROID["International_1924",6378388.0,297.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Gauss_Kruger"],PARAMETER["False_Easting",2500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",24.0],PARAMETER["Scale_Factor",1.0],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]',
      '2393'='PROJCS["Finland_Zone_3",GEOGCS["GCS_KKJ",DATUM["D_KKJ",SPHEROID["International_1924",6378388.0,297.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Gauss_Kruger"],PARAMETER["False_Easting",3500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",27.0],PARAMETER["Scale_Factor",1.0],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]',
      '3009'='PROJCS["SWEREF99 13 30",GEOGCS["SWEREF99",DATUM["D_SWEREF99",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",13.5],PARAMETER["scale_factor",1],PARAMETER["false_easting",150000],PARAMETER["false_northing",0],UNIT["Meter",1]]'
      '3067'='PROJCS["Europe ETRS-TM35, ETRS89 datum; 24 deg East to 30 deg East",GEOGCS["ETRS89",DATUM["D_ETRS89",SPHEROID["World_Geodetic_System_of_1984_GEM_10C",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",27],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3776'='PROJCS["3TM114-83",GEOGCS["North_American_Datum_1983",DATUM["D_North_American_1983",SPHEROID["Geodetic Reference System of 1980",6378137.0,298.2572221009113]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",-114.0],PARAMETER["scale_factor",0.9999],PARAMETER["latitude_of_origin",0.0],UNIT["Meter",1.0]]',
      '3857'='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]',
      '3877'='PROJCS["ETRS89_ETRS_GK23FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",23],PARAMETER["scale_factor",1],PARAMETER["false_easting",23500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3878'='PROJCS["ETRS89_ETRS_GK24FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",24],PARAMETER["scale_factor",1],PARAMETER["false_easting",24500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3879'='PROJCS["ETRS89_ETRS_GK25FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",25],PARAMETER["scale_factor",1],PARAMETER["false_easting",25500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '3880'='PROJCS["ETRS89_ETRS_GK26FIN_2010",GEOGCS["GCS_ETRS89",DATUM["D_ETRS_1989",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221008916]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",26],PARAMETER["scale_factor",1],PARAMETER["false_easting",26500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '4258'='GEOGCS["Europe ETRS 89 (EUREF 89 System), Latitude-Longitude; Degrees",DATUM["D_ETRS89",SPHEROID["World_Geodetic_System_of_1984_GEM_10C",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]',
      '4326'='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]',
      '28992'='PROJCS["RD_New",GEOGCS["GCS_Amersfoort",DATUM["D_Amersfoort",SPHEROID["Bessel_1841",6377397.155,299.1528128]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199432955]],PROJECTION["Double_Stereographic"],PARAMETER["False_Easting",155000],PARAMETER["False_Northing",463000],PARAMETER["Central_Meridian",5.38763888888889],PARAMETER["Scale_Factor",0.9999079],PARAMETER["Latitude_Of_Origin",52.15616055555555],UNIT["Meter",1]]',
      '50001'='PROJCS["Finnish KKJ 3 (YKJ)",GEOGCS["ERP50-FI",DATUM["D_ERP50-FI",SPHEROID["Hayford_1924_aka_1909_same_as_International_1924",6378388,297.0000000000601]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",27],PARAMETER["scale_factor",1],PARAMETER["false_easting",3500000],PARAMETER["false_northing",0],UNIT["Meter",1]]',
      '99999'=''
      )[as.character(epsg)]

#' Transform and convert between projections.
#' @param coords a data frame with columns "x" and "y" or
#' a two-column matrix of X- and Y-coordinates or an \pkg{sp} object.
#' @param from EPSG code of current projection.
#' @param to EPSG code of desired projection.
#' @param names names of columns holding coordinates.
#' (Only relevant if \code{coords} is a data frame.)
#' @param fun function to apply to resulting coordinates (e.g. round).
#' (Only used if \code{coords} is a data.frame or a matrix.)
#' @return A data frame or two-column matrix or an \pkg{sp} object.
#' @rdname reproject
#' @export
reproject = function(coords, from, to, names=c("x", "y"), fun=identity)
    UseMethod("reproject", coords)

#' @rdname reproject
#' @method reproject data.frame
#' @export
reproject.data.frame = function(coords, from, to, names=c("x", "y"),
                                fun=identity) {
    
    from = proj.proj4(from)
    to = proj.proj4(to)
    stopifnot(is.character(from))
    stopifnot(is.character(to))
    if (from == to)
        return(coords)
    xname = names[1]
    yname = names[2]
    m = !is.na(coords[,xname]) & !is.na(coords[,yname])
    shapes = sp::SpatialPoints(cbind(coords[m,xname], coords[m,yname]), sp::CRS(from))
    shapes = sp::spTransform(shapes, sp::CRS(to))
    coords[m,xname] = fun(shapes@coords[,1])
    coords[m,yname] = fun(shapes@coords[,2])
    coords[!m,xname] = NA
    coords[!m,yname] = NA
    return(coords)
}

#' @rdname reproject
#' @method reproject matrix
#' @export
reproject.matrix = function(coords, from, to, names=NULL, fun=identity) {
    from = proj.proj4(from)
    to = proj.proj4(to)
    stopifnot(is.character(from))
    stopifnot(is.character(to))
    if (from == to)
        return(coords)
    m = !is.na(coords[,1]) & !is.na(coords[,2])
    shapes = sp::SpatialPoints(cbind(coords[m,1], coords[m,2]), sp::CRS(from))
    shapes = sp::spTransform(shapes, sp::CRS(to))
    coords[m,1] = fun(shapes@coords[,1])
    coords[m,2] = fun(shapes@coords[,2])
    coords[!m,1] = NA
    coords[!m,2] = NA
    return(coords)
}

reproject.sp = function(coords, from, to, names=NULL, fun=NULL) {
    from = proj.proj4(from)
    to = proj.proj4(to)
    stopifnot(is.character(from))
    stopifnot(is.character(to))
    if (from == to)
        return(coords)
    sp::proj4string(coords) = sp::CRS(from)
    return(sp::spTransform(coords, sp::CRS(to)))
}

#' @rdname reproject
#' @method reproject SpatialLinesDataFrame
#' @export
reproject.SpatialLinesDataFrame = reproject.sp

#' @rdname reproject
#' @method reproject SpatialPointsDataFrame
#' @export
reproject.SpatialPointsDataFrame = reproject.sp

#' @rdname reproject
#' @method reproject SpatialPolygonsDataFrame
#' @export
reproject.SpatialPolygonsDataFrame = reproject.sp
>>>>>>> e152c150a1604128719bd69d56e79ba2d58651f6
