context("test-select.R")

test_that("select() works", {
  iris <- as_tibble(datasets::iris)
  test <- FDF(iris)
  expect_identical(as_tibble(select(test, 1:4)), iris[1:4])
  expect_identical(as_tibble(select(test, -3)),  iris[-3])
  expect_identical(as_tibble(select(test, c("Sepal.Length", "Sepal.Width"))),
                             iris[c("Sepal.Length", "Sepal.Width")])
  expect_identical(as_tibble(select(test, Sepal.Length, Sepal.Width)),
                   iris[c("Sepal.Length", "Sepal.Width")])

  expect_s4_class(dplyr::select(test, 1:4), "FDF")
})
