################################################################################

context("test-mutate.R")

################################################################################

test_that("add_columns() works", {
  test <- FDF(iris <- datasets::iris)
  test2 <- test$add_columns(iris)
  expect_identical(dim(test2), dim(iris))
  expect_identical(test2$ind_col, set_names(6:10, names(iris)))
  expect_identical(test2$types, rep(test$types, 2))
  expect_identical(extract_numeric(test2$address, 5),
                   extract_numeric(test2$address, 10))
  expect_identical(as_tibble(test2),
                   dplyr::mutate_if(as_tibble(iris), is.factor, as.character))
  iris2 <- rev(stats::setNames(iris, paste0(names(iris), 2)))
  expect_error(test$add_columns(iris2), "Inconsistency")
  test3 <- test2$add_columns(iris2)
  expect_identical(test3$types, c(test2$types, rev(test$types)))
  expect_identical(test3$ind_col, set_names(6:15, c(names(iris), names(iris2))))
  iris_binded <- dplyr::mutate_if(cbind(iris, iris2), is.factor, as.character)
  expect_identical(as_tibble(test3), as_tibble(iris_binded))
})

################################################################################

test_that("arrange() works", {
  iris <- as_tibble(mutate(datasets::iris, Species = as.character(Species)))
  test <- FDF(iris)
  test2 <- arrange(test, Sepal.Length)
  expect_identical(as_tibble(test2), arrange(iris, Sepal.Length))
  test3 <- arrange(test, Species, desc(Sepal.Length))
  expect_identical(as_tibble(test3), arrange(iris, Species, desc(Sepal.Length)))

  test4 <- filter(test2, Species == "setosa")
  expect_identical(pull(test4, Species), rep("setosa", 50))
  expect_false(is.unsorted(pull(test4, Sepal.Length)))

  expect_s4_class(dplyr::arrange(test, Sepal.Length), "FDF")
})

################################################################################
