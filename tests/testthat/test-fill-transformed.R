################################################################################

context("test-fill-transformed.R")

################################################################################

test_that("transform_chr() works", {
  fake <- rlang::env(nstr = 1L, strings = rep(NA_character_, NSTR_MAX))
  df_j <- as.character(iris$Species)
  expect_identical(transform_chr(fake, df_j, 5), rep(2:4, each = 50))
  expect_identical(transform_chr(fake, df_j, 5), rep(2:4, each = 50))
  df_j2 <- paste0(df_j, 2)
  expect_identical(transform_chr(fake, df_j2, 5), rep(5:7, each = 50))
  expect_identical(transform_chr(fake, df_j2, 5), rep(5:7, each = 50))
  df_j2[] <- NA_character_
  expect_identical(transform_chr(fake, df_j2, 5), rep(1L, 150))
  fake <- rlang::env(nstr = 1L, strings = rep(NA_character_, NSTR_MAX))
  df_j <- as.character(iris$Species)
  df_j[df_j == "virginica"] <- NA
  expect_identical(transform_chr(fake, df_j, 5), rep(c(2:3, 1L), each = 50))
  expect_identical(transform_chr(fake, df_j, 5), rep(c(2:3, 1L), each = 50))
})

################################################################################

test_that("transform_fct() works", {
  fake <- rlang::env(nstr = 1L, strings = rep(NA_character_, NSTR_MAX))
  df_j <- datasets::iris$Species
  expect_identical(transform_fct(fake, df_j), 1:3)
  expect_identical(transform_fct(fake, df_j), 1:3)
  levels(df_j) <- paste0(levels(df_j), 2)
  expect_identical(transform_fct(fake, df_j), 4:6)
  expect_identical(transform_fct(fake, df_j), 4:6)
  df_j[1] <- NA_character_
  expect_identical(transform_fct(fake, df_j), 4:6)
  fake <- rlang::env(nstr = 1L, strings = rep(NA_character_, NSTR_MAX))
  df_j <- iris$Species
  df_j[df_j == "virginica"] <- NA
  expect_identical(transform_fct(fake, df_j), 1:3)
  expect_identical(transform_fct(fake, df_j), 1:3)

  iris <- dplyr::mutate_at(datasets::iris, 1:4, as.character)
  test <- FDF(iris)
  expect_identical(as_tibble(test), as_tibble(iris))
})

################################################################################
