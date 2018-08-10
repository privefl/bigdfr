context("test-delayed-assign.R")

test_that("delayedAssign() works", {
  e <- new.env()
  error <- "This error is delayed."
  delayedAssign("error", stop(error), assign.env = e)
  e$aa <- 2
  e$bb <- iris
  expect_identical(e$aa, 2)
  expect_identical(e$bb, iris)
  expect_error(e$error, error, fixed = TRUE)
})
