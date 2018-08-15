context("test-FDF-extract.R")

test_that("extract works", {
  test <- FDF(iris <- datasets::iris)
  # With internal extract
  expect_identical(extract_dbl(test$address, 1, list(test$ind_row))[[1]],
                   iris[[1]])
  expect_identical(extract_ushort(test$address, 5, list(test$ind_row))[[1]],
                   as.integer(iris[[5]]))
  expect_identical(
    extract_string(test$address, 5, list(test$ind_row), test$strings)[[1]],
    as.character(iris[[5]]))
  # With exported pull
  expect_identical(pull(test, 2), iris[[2]])
  expect_identical(pull(test, 5), as.character(iris[[5]]))
})
