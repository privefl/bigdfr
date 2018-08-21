################################################################################

context("test-mutate.R")

################################################################################

test_that("add_columns() works", {
  test <- FDF(iris <- datasets::iris)
  test2 <- test$add_columns(iris)
  expect_identical(dim(test2), dim(iris))
  expect_identical(test2$ind_col, set_names(6:10, names(iris)))
  expect_identical(test2$types, rep(test$types, 2))
  expect_identical(extract_ushort(test2$address, 5,  list(test2$ind_row)),
                   extract_ushort(test2$address, 10, list(test2$ind_row)))
  expect_identical(as_tibble(test2), as_tibble(iris))
  iris2 <- rev(stats::setNames(iris, paste0(names(iris), 2)))
  expect_error(test$add_columns(iris2), "Inconsistency")
  test3 <- test2$add_columns(iris2)
  expect_identical(test3$types, c(test2$types, rev(test$types)))
  expect_identical(test3$ind_col, set_names(6:15, c(names(iris), names(iris2))))
  expect_identical(as_tibble(test3), as_tibble(cbind(iris, iris2)))
})

################################################################################

test_that("mutate() works", {
  test <- FDF(datasets::airquality)
  test2 <- mutate(test, Temp_Celsius = (Temp - 32) / 1.8,
                  Temp_Kelvin = Temp_Celsius + 273.15)
  airquality2 <- mutate(datasets::airquality, Temp_Celsius = (Temp - 32) / 1.8,
                        Temp_Kelvin = Temp_Celsius + 273.15)
  expect_identical(as_tibble(test2), as_tibble(airquality2))

  test <- FDF(datasets::airquality)
  test2 <- dplyr::mutate(test, Temp_Celsius = (Temp - 32) / 1.8)
  expect_s4_class(test2, "FDF")
})

################################################################################
