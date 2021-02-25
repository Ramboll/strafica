# -*- coding: us-ascii-unix -*-
context("test-proj")

test_that("projections work", {
    points = read.delims("test-proj.txt")
    for (i in rows.along(points)) {
        for (j in setdiff(which(points$place == points$place[i]), i)) {
            out = reproject(points[i,,drop=FALSE],
                            from=points$epsg[i],
                            to=points$epsg[j])

            error = eucd(out$x, out$y, points$x[j], points$y[j])
            if (out$x > 1000) {
                # Metric coordinates
                messagef("%s, %s -> %s, error: %.6f meters",
                         points$place[i],
                         points$epsg[i],
                         points$epsg[j],
                         error)

                expect_false(error > 3)
            } else {
                # Degrees
                messagef("%s, %s -> %s, error: %.6f degrees",
                         points$place[i],
                         points$epsg[i],
                         points$epsg[j],
                         error)

                expect_false(error > 0.0001)
            }
        }
    }
})
