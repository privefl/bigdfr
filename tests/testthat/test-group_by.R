################################################################################

context("test-group_by.R")

################################################################################

test_that("group_by() works", {

  test <- FDF(iris <- datasets::iris)
  test2 <- group_by(test, Species)
  expect_s3_class(test$groups, "tbl_df")
  expect_equal(dim(test$groups), c(0, 0))
  expect_identical(test2$groups,
                   dplyr::tibble(Species = levels(iris$Species),
                                 ind_row = list(0:49, 50:99, 100:149)))

  test <- FDF(mtcars <- datasets::mtcars)
  test2 <- group_by(test, cyl, vs, am)
  expect_s3_class(test$groups, "tbl_df")
  expect_equal(dim(test$groups), c(0, 0))
  expect_identical(test2$groups,
                   mutate(mtcars, id = 1:n() - 1L) %>%
                     group_by(cyl, vs, am) %>%
                     dplyr::summarise(ind_row = list(id)) %>%
                     dplyr::ungroup())

  expect_s4_class(dplyr::group_by(test, cyl, vs, am), "FDF")
})

################################################################################
