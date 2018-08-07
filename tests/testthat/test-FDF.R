context("test-FDF.R")

test_that("FDF initialization works", {
  test <- FDF(iris)
  expect_equal(file.size(test$backingfile), 150 * (4 * 8 + 1 * 2))
  iris$osef <- list(1)
  expect_error(FDF(iris), ERROR_TYPE, fixed = TRUE)
})
