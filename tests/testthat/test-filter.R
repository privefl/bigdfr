context("test-filter.R")

test_that("filter() works", {
  iris <- as_tibble(mutate(datasets::iris, Species = as.character(Species)))
  test <- FDF(iris)
  test2 <- filter(test, Species == "setosa")
  expect_identical(test2$ind_row, 1:50)
  expect_identical(test2$nrow_all, 150L)
  expect_identical(as_tibble(test2)$Species, rep("setosa", 50))
  expect_identical(as_tibble(test2), filter(iris, Species == "setosa"))
  test3 <- filter(test, Species == "setosa", Sepal.Length < 5)
  expect_identical(as_tibble(test3), as_tibble(filter(test2, Sepal.Length < 5)))
  expect_identical(as_tibble(test3), filter(iris, Species == "setosa", Sepal.Length < 5))
})
