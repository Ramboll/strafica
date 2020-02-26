# -*- coding: us-ascii-unix -*-
context("test-operators")

test_that("operators work", {
    x = c("alpha","beta","gamma")
    # testing in
    expect_true("beta" %in% x)
    expect_false("delta" %in% x)
    # testing nin
    expect_false("beta" %nin% x)
    expect_true("delta" %nin% x)
})

test_that("redesigned nin works the same", {
    
    # old design of nin
    `%oldnin%` = Negate(`%in%`)
    
    # normal vector
    x = c("alpha","beta","gamma")
    expect_identical("beta" %nin% x, "beta" %oldnin% x)
    expect_identical("delta" %nin% x, "delta" %oldnin% x)
    expect_identical(NA %nin% x, NA %oldnin% x)
    expect_identical(NaN %nin% x, NaN %oldnin% x)
    expect_identical(NULL %nin% x, NULL %oldnin% x)
    
    # vector with NA
    x = c("alpha","beta","gamma",NA)
    expect_identical("beta" %nin% x, "beta" %oldnin% x)
    expect_identical("delta" %nin% x, "delta" %oldnin% x)
    expect_identical(NA %nin% x, NA %oldnin% x)
    expect_identical(NaN %nin% x, NaN %oldnin% x)
    expect_identical(NULL %nin% x, NULL %oldnin% x)
    
    # vector with NA and NaN
    x = c("alpha","beta","gamma",NA,NaN)
    expect_identical("beta" %nin% x, "beta" %oldnin% x)
    expect_identical("delta" %nin% x, "delta" %oldnin% x)
    expect_identical(NA %nin% x, NA %oldnin% x)
    expect_identical(NaN %nin% x, NaN %oldnin% x)
    expect_identical(NULL %nin% x, NULL %oldnin% x)
    
    # vector with NA, NaN, and NULL
    x = c("alpha","beta","gamma",NA,NaN,NULL)
    expect_identical("beta" %nin% x, "beta" %oldnin% x)
    expect_identical("delta" %nin% x, "delta" %oldnin% x)
    expect_identical(NA %nin% x, NA %oldnin% x)
    expect_identical(NaN %nin% x, NaN %oldnin% x)
    expect_identical(NULL %nin% x, NULL %oldnin% x)
})
