# -*- coding: us-ascii-unix -*-
remove_all_shape_file_parts = function(main_shapefile) {
    extensions = c(".dbf", ".prj", ".shp", ".shx")
    shapefile_parts = sapply(
        extensions,
        function(extension) {
            gsub("\\.shp$", extension, main_shapefile)
        }
    )
    for (shapefile_part in shapefile_parts) {
        if (file.exists(shapefile_part)) {
            file.remove(shapefile_part)
        }
    }
}

get_dummy_lines = function() {
    # Creating lines by creating first 
    # segments (a dataframe with custom columns)
    seg = data.frame(eid = 1, sid = 1,
                     ix = 1, iy = 1,
                     jx = 2, jy = 2)
    data = data.frame(eid = 1, value = 1337)
    lines = list(lines = segments.to.lines(seg), data = data)
    return(lines)
}

get_dummy_lines_as_sp = function() {
    dummy_lines = get_dummy_lines()
    # Convert lines to a SpatialLinesDataFrame
    lines_as_spdf = lines.to.sp(dummy_lines$lines,
                                data=dummy_lines$data,
                                verbose=FALSE)
    return(lines_as_spdf)
}

test_that("verbose=FALSE option can suppress messages with sp.to.lines", {
    dummy_lines_spdf = get_dummy_lines_as_sp()
    expect_message(
        strafica::sp.to.lines(dummy_lines_spdf),
        NULL
    )
    expect_message(
        strafica::sp.to.lines(dummy_lines_spdf, verbose = FALSE),
        NA
    )
})

test_that("verbose=FALSE option can suppress messages with lines.to.sp", {
    dummy_lines = get_dummy_lines()
    expect_message(
        strafica::lines.to.sp(
            dummy_lines$lines,
            dummy_lines$data
        ),
        NULL
    )
    expect_message(
        strafica::lines.to.sp(
            dummy_lines$lines,
            dummy_lines$data,
            verbose = FALSE
        ),
        NA
    )
})
