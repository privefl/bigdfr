context("test-group_by.R")

test_that("group_by() works", {
  test <- FDF(iris <- datasets::iris)
  test2 <- group_by(test, Species)
  expect_s3_class(test$groups, "tbl_df")
  expect_equal(dim(test$groups), c(1, 1))
  expect_identical(test2$groups,
                   dplyr::tibble(Species = levels(iris$Species),
                                 rel_ind_row = list(1:50, 51:100, 101:150)))

  test <- FDF(mtcars <- datasets::mtcars)
  test2 <- group_by(test, cyl, vs, am)
  expect_s3_class(test$groups, "tbl_df")
  expect_equal(dim(test$groups), c(1, 1))
  expect_identical(test2$groups,
                   mutate(mtcars, id = 1:n()) %>%
                     group_by(cyl, vs, am) %>%
                     dplyr::summarise(rel_ind_row = list(id)) %>%
                     dplyr::ungroup())
})


