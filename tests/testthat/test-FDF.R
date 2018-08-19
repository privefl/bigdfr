################################################################################

context("test-FDF.R")

################################################################################

test_that("FDF initialization works", {

  # First init, then copy doesn't change previous object
  (test <- FDF(iris <- mutate(datasets::iris, is_setosa = Species == "setosa")))
  expect_false(identical(test$address, methods::new("externalptr")))
  test0 <- test$copy(nstr = 19L)
  expect_true(identical(test0$extptr, methods::new("externalptr")))
  expect_identical(test0$nstr, 19L)
  expect_identical(test$nstr, 4L)

  # $copy() don't really copy until modified
  expect_identical(data.table::address(test$ind_row),
                   data.table::address(test0$ind_row))
  pull(test0, 4)
  expect_identical(data.table::address(test$ind_row),
                   data.table::address(test0$ind_row))
  test0$ind_row[1] <- 1L
  expect_failure(expect_identical(data.table::address(test$ind_row),
                                  data.table::address(test0$ind_row)))

  # Same dim/nrow/ncol/length
  expect_s4_class(test, "FDF")
  expect_equal(dim(test),    dim(iris))
  expect_equal(nrow(test),   nrow(iris))
  expect_equal(ncol(test),   ncol(iris))
  expect_equal(length(test), length(iris))

  # Check sizes and values
  expect_equal(file.size(test$backingfile), 150 * (4 * 8 + 1 * 2 + 1 * 4))
  iris$osef <- list(1)
  expect_error(FDF(iris), ERROR_TYPE, fixed = TRUE)
  expect_error(FDF(mtcars)$save(tempfile()), "must be '.rds'")
  expect_s4_class(test2 <- FDF(mtcars)$save(tempfile(fileext = ".rds")), "FDF")
  expect_true(test2$is_saved)

  expect_equal(readBin(test$backingfile, what = 0, n = 150 * 4),
               unlist(iris[1:4]), check.attributes = FALSE)
  read_ushort <- readBin(test$backingfile, what = 1L, size = 2,  n = 10e3)
  expect_identical(head(tail(read_ushort, 150 * 3), 150), as.integer(iris$Species))
  expect_identical(test$nstr, 4L)
  expect_identical(test$strings, c(NA, levels(iris$Species), rep(NA, 2^16 - 4)))
})

################################################################################

test_that("$as_env() works", {
  test <- FDF(iris <- mutate(datasets::iris, Species = as.character(Species)))
  expect_identical(mget(names(iris), envir = test$as_env()), as.list(iris))
})

################################################################################
