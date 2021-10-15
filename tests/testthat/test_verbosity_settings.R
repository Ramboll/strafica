# -*- coding: us-ascii-unix -*-
remove_all_shape_file_parts = function(main_shapefile) {
    extensions = c(".dbf", ".prj", ".shp", ".shx")
    distnct_shapefile_parts = sapply(
        extensions,
        function(extension) {
            gsub("\\.shp$", extension, main_shapefile)
        }
    )
    for (shapefile_part in distnct_shapefile_parts) {
        if (file.exists(shapefile_part)) {
            # Delete file if it exists
            file.remove(shapefile_part)
        }
    }
}

test_that("with verbosity unset, there will be messages when writing out a shapefile", {
    dummy_epsg_code = 3067
    spatial_data_frame_lines = strafica::read.shape(
        "../data/one_line.shp",
        verbose = FALSE
    )
    dummy_file_name_shp = tempfile(fileext = ".shp")
    expect_message(
        strafica::write.shape(
            spatial_data_frame_lines,
            dummy_file_name_shp,
            epsg = dummy_epsg_code
        ),
        NULL
    )
    remove_all_shape_file_parts(dummy_file_name_shp)
})

test_that("with verbosity=FALSE, there will be no messages when writing out a shapefile", {
    dummy_epsg_code = 3067
    spatial_data_frame_lines = strafica::read.shape(
        "../data/one_line.shp",
        verbose = FALSE
    )
    dummy_file_name_shp = tempfile(fileext = ".shp")
    expect_message(
        strafica::write.shape(
            spatial_data_frame_lines,
            dummy_file_name_shp,
            epsg = dummy_epsg_code,
            verbose = FALSE
        ),
        NA
    )
    remove_all_shape_file_parts(dummy_file_name_shp)
})


test_that("verbosity=FALSE option can suppress messages with sp.to.lines", {
    spatial_data_frame_lines = strafica::read.shape(
        "../data/one_line.shp",
        verbose = FALSE,
    )
    expect_message(
        strafica::sp.to.lines(spatial_data_frame_lines),
        NULL
    )
    expect_message(
        strafica::sp.to.lines(spatial_data_frame_lines, verbose = FALSE),
        NA
    )
})

test_that("verbosity=FALSE option can suppress messages with lines.to.sp", {
    spatial_data_frame_lines = strafica::read.shape(
        "../data/one_line.shp",
        verbose = FALSE
    )
    lines_as_two_data_frames = strafica::sp.to.lines(spatial_data_frame_lines)
    expect_message(
        strafica::lines.to.sp(
            lines_as_two_data_frames$lines,
            lines_as_two_data_frames$data
        ),
        NULL
    )
    expect_message(
        strafica::lines.to.sp(
            lines_as_two_data_frames$lines,
            lines_as_two_data_frames$data,
            verbose = FALSE
        ),
        NA
    )
})

test_that("verbosity=FALSE option can suppress output with sp.to.polys", {
    spatial_data_frame_polys = strafica::read.gpkg(
        "../data/two_polygons.gpkg",
        verbose = FALSE,
    )
    expect_message(
        strafica::sp.to.polys(spatial_data_frame_polys),
        NULL
    )
    expect_message(
        strafica::sp.to.polys(spatial_data_frame_polys, verbose = FALSE),
        NA
    )
})

test_that("verbosity=FALSE option can suppress output with polys.to.sp", {
    spatial_data_frame_polys = strafica::read.gpkg(
        "../data/two_polygons.gpkg",
        verbose = FALSE,
    )
    spatial_polygons_as_list = strafica::sp.to.polys(spatial_data_frame_polys)
    expect_message(
        strafica::polys.to.sp(
            spatial_polygons_as_list$outlines,
            spatial_polygons_as_list$data,
        ),
        NULL
    )
    expect_message(
        strafica::polys.to.sp(
            spatial_polygons_as_list$outlines,
            spatial_polygons_as_list$data,
            verbose = FALSE
        ),
        NA
    )
})