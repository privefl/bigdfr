context("test-utils-assert.R")

test_that("assert_ext() works", {
  expect_error(assert_ext("toto",             "toto"), "must be '.toto'")
  expect_error(assert_ext("totords",          "toto"), "must be '.toto'")
  expect_error(assert_ext("toto.rds2",        "toto"), "must be '.toto'")
  expect_error(assert_ext("toto.toto.bidule", "toto"), "must be '.toto'")
  expect_null(assert_ext("toto.toto", "toto"))
})
