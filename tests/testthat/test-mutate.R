context("test-mutate.R")

test_that("add_columns() works", {
  test <- FDF(iris <- datasets::iris)
  test2 <- test$add_columns(iris)
  expect_identical(dim(test2), dim(iris))
  expect_identical(test2$ind_col, stats::setNames(6:10, names(iris)))
  expect_identical(test2$types, rep(test$types, 2))
  expect_identical(extract_numeric(test2$address, 5),
                   extract_numeric(test2$address, 10))
  expect_identical(as_tibble(test2),
                   dplyr::mutate_if(as_tibble(iris), is.factor, as.character))
})
