################################################################################

context("test-mutate.R")

################################################################################

test_that("add_columns() works", {
  test <- FDF(iris <- datasets::iris)
  test2 <- test$add_columns(iris)
  expect_identical(dim(test2), dim(iris))
  expect_identical(test2$ind_col, set_names(6:10, names(iris)))
  expect_identical(test2$types, rep(test$types, 2))
  expect_identical(extract_numeric(test2$address, 5),
                   extract_numeric(test2$address, 10))
  expect_identical(as_tibble(test2),
                   dplyr::mutate_if(as_tibble(iris), is.factor, as.character))
  iris2 <- rev(stats::setNames(iris, paste0(names(iris), 2)))
  expect_error(test$add_columns(iris2), "Inconsistency")
  test3 <- test2$add_columns(iris2)
  expect_identical(test3$types, c(test2$types, rev(test$types)))
  expect_identical(test3$ind_col, set_names(6:15, c(names(iris), names(iris2))))
  iris_binded <- dplyr::mutate_if(cbind(iris, iris2), is.factor, as.character)
  expect_identical(as_tibble(test3), as_tibble(iris_binded))
})

################################################################################

test_that("mutate() works", {
  test <- FDF(datasets::airquality)
  test2 <- mutate(test, Temp_Celsius = (Temp - 32) / 1.8,
                  Temp_Kelvin = Temp_Celsius + 273.15)
  airquality2 <- mutate(datasets::airquality, Temp_Celsius = (Temp - 32) / 1.8,
                        Temp_Kelvin = Temp_Celsius + 273.15)
  expect_identical(as_tibble(test2), as_tibble(airquality2))
})

################################################################################
