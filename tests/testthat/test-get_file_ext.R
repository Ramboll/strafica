test_that("get_file_ext works", {
    path1 = "../../test.txt"
    path2 = "do.not.put.dots.in.dir.names/file.txt"
    path3 = "file.gpkg"
    expect_identical(get_file_ext(path1), ".txt")
    expect_identical(get_file_ext(path2), ".txt")
    expect_identical(get_file_ext(path3), ".gpkg")
})
