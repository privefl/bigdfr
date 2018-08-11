################################################################################

context("test-pull.R")

################################################################################

test_that("pull() works", {
  iris <- datasets::iris
  iris$Species <- as.character(iris$Species)
  test <- FDF(iris)
  expect_identical(pull(test, 1),       iris[[1]])
  expect_identical(pull(test, 5),       iris[[5]])
  expect_identical(pull(test, -1),      iris[[5]])
  expect_identical(pull(test, Species), iris[[5]])
  expect_error(pull(test, 1:2))
})

################################################################################

test_that("pull() works with filter()", {
  iris <- datasets::iris
  iris$Species <- as.character(iris$Species)
  test <- filter(FDF(iris), Species == "virginica")
  expect_identical(test$ind_row, 101:150)
  iris_virginica <- filter(iris, Species == "virginica")
  expect_identical(pull(test, 1),       iris_virginica[[1]])
  expect_identical(pull(test, 4),       iris_virginica[[4]])
  expect_identical(pull(test, 5),       iris_virginica[[5]])
  expect_identical(pull(test, -1),      iris_virginica[[5]])
  expect_identical(pull(test, Species), iris_virginica[[5]])
  expect_error(pull(test, 1:2))
})

################################################################################

test_that("pull() works with subset()", {
  iris <- datasets::iris
  iris$Species <- as.character(iris$Species)
  iris_subset <- iris[subset <- sample(5, 3)]
  test <- select(FDF(iris), subset)
  expect_identical(test$ind_col, set_names(subset, names(iris_subset)))
  expect_identical(pull(test, 1),  iris_subset[[1]])
  expect_identical(pull(test, -1), iris_subset[[3]])
  expect_error(pull(test, 1:2))
})

################################################################################
