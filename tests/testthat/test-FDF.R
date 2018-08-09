context("test-FDF.R")

test_that("FDF initialization works", {

  test0 <- FDF(iris <- datasets::iris)
  test <- test0$copy()
  test0$nstr <- 19L
  expect_identical(test$nstr, 3L)

  expect_s4_class(test, "FDF")
  expect_equal(dim(test),    dim(iris))
  expect_equal(nrow(test),   nrow(iris))
  expect_equal(ncol(test),   ncol(iris))
  expect_equal(length(test), length(iris))

  expect_equal(file.size(test$backingfile), 150 * (4 * 8 + 1 * 2))
  iris$osef <- list(1)
  expect_error(FDF(iris), ERROR_TYPE, fixed = TRUE)
  expect_error(FDF(mtcars)$save(tempfile()), "must be '.rds'")
  expect_s4_class(test2 <- FDF(mtcars)$save(tempfile(fileext = ".rds")), "FDF")
  expect_true(test2$is_saved)

  expect_equal(readBin(test$backingfile, what = 0, n = 150 * 4),
               unlist(iris[1:4]), check.attributes = FALSE)
  read_ushort <- readBin(test$backingfile, what = 1L, size = 2,  n = 10e3)
  expect_identical(tail(read_ushort, 150), as.integer(iris$Species) - 1L)
  expect_identical(test$nstr, 3L)
  expect_identical(test$strings, c(levels(iris$Species), rep(NA, 2^16 - 3)))
})
