context("test-FDF.R")

test_that("FDF initialization works", {
  iris <- datasets::iris
  test <- FDF(iris)
  expect_s4_class(test, "FDF")
  expect_equal(file.size(test$backingfile), 150 * (4 * 8 + 1 * 2))
  iris$osef <- list(1)
  expect_error(FDF(iris), ERROR_TYPE, fixed = TRUE)
  expect_error(FDF(mtcars)$save(tempfile()), "must be '.rds'")
  expect_s4_class(test2 <- FDF(mtcars)$save(tempfile(fileext = ".rds")), "FDF")
  expect_true(test2$is_saved)
})
