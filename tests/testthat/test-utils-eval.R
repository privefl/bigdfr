context("test-utils-eval.R")

test_that("get_call_names() works", {
  myquos <- quos(
    1 + mean(sqrt(Sepal.Length), na.rm = TRUE, trim = 0.1),
    1 + mean(sqrt(2), na.rm = T, trim = 0.1),
    Sepal.Width = plus_one(Sepal.Length),
    Sepal.Width = 1
  )
  expect_identical(
    lapply(myquos, get_call_names),
    list("Sepal.Length", "T", Sepal.Width = "Sepal.Length", Sepal.Width = NULL)
  )
})
