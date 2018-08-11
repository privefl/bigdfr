################################################################################

context("test-fill-transformed.R")

################################################################################

test_that("transform_chr() works", {
  fake <- rlang::env(nstr = 1L, strings = rep(NA_character_, NSTR_MAX))
  df_j <- as.character(iris$Species)
  expect_identical(transform_chr(fake, df_j), rep(1:3, each = 50))
  expect_identical(transform_chr(fake, df_j), rep(1:3, each = 50))
  df_j2 <- paste0(df_j, 2)
  expect_identical(transform_chr(fake, df_j2), rep(4:6, each = 50))
  expect_identical(transform_chr(fake, df_j2), rep(4:6, each = 50))
  df_j2[] <- NA_character_
  expect_identical(transform_chr(fake, df_j2), rep(0L, 150))
  fake <- rlang::env(nstr = 1L, strings = rep(NA_character_, NSTR_MAX))
  df_j <- as.character(iris$Species)
  df_j[df_j == "virginica"] <- NA
  expect_identical(transform_chr(fake, df_j), rep(c(1:2, 0L), each = 50))
  expect_identical(transform_chr(fake, df_j), rep(c(1:2, 0L), each = 50))
})

################################################################################

test_that("transform_fct() works", {
  fake <- rlang::env(nstr = 1L, strings = rep(NA_character_, NSTR_MAX))
  df_j <- datasets::iris$Species
  expect_identical(transform_fct(fake, df_j), rep(1:3, each = 50))
  expect_identical(transform_fct(fake, df_j), rep(1:3, each = 50))
  levels(df_j) <- paste0(levels(df_j), 2)
  expect_identical(transform_fct(fake, df_j), rep(4:6, each = 50))
  expect_identical(transform_fct(fake, df_j), rep(4:6, each = 50))
  df_j[1] <- NA_character_
  expect_identical(transform_fct(fake, df_j)[1], 0L)
  fake <- rlang::env(nstr = 1L, strings = rep(NA_character_, NSTR_MAX))
  df_j <- iris$Species
  df_j[df_j == "virginica"] <- NA
  expect_identical(transform_fct(fake, df_j), rep(c(1:2, 0L), each = 50))
  expect_identical(transform_fct(fake, df_j), rep(c(1:2, 0L), each = 50))
})

################################################################################
