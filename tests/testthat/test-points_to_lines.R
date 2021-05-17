# -*- coding: utf-8-unix -*-
test_that("points_to_lines works", {
    points = read.delims("points_to_lines_in.txt")
    expected_lines = read.delims("points_to_lines_out.txt")
    lines = points_to_lines(points, "trip_id")
    expect_equal(lines$lines, expected_lines)
})
