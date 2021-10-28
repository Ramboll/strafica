not_a_segment = data.frame(
    x = c(0, 0),
    y = c(0, 1)
)

test_that("flip_segments throws an error when no segments given", {
  expect_error(flip_segments(not_a_segment))
})

segments = data.frame(
    ix = c(0, 0),
    iy = c(0, 1),
    jx = c(0, 1),
    jy = c(1, 1)
)

segments_flipped = data.frame(
    ix = c(0, 1),
    iy = c(1, 1),
    jx = c(0, 0),
    jy = c(0, 1)
)

test_that("flip_segments flips correctly", {
    expect_equal(flip_segments(segments),
                 segments_flipped)
})
