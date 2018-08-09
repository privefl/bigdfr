context("test-FDF-extract.R")

test_that("extract works", {
  iris <- datasets::iris
  test <- FDF(iris)
  # With internal extract
  expect_identical(extract_numeric(test$address, 1), iris[[1]])
  expect_identical(extract_numeric(test$address, 5), as.integer(iris[[5]]) - 1L)
  expect_identical(extract_string(test$address, 5, test$strings),
                   as.character(iris[[5]]))
  # With exported pull
  expect_identical(pull(test, 2), iris[[2]])
  expect_identical(pull(test, 5), as.character(iris[[5]]))
})