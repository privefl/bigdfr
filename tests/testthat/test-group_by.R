################################################################################

context("test-group_by.R")

################################################################################

test_that("group_by() works", {

  test <- FDF(iris <- datasets::iris)
  test2 <- group_by(test, Species)
  expect_s3_class(test$groups, "tbl_df")
  expect_equal(dim(test$groups), c(1, 1))
  expect_identical(test2$groups,
                   dplyr::tibble(Species = levels(iris$Species),
                                 ind_row = list(0:49, 50:99, 100:149)))
  expect_identical(dim(group_by(test2, Petal.Length)$groups),
                   c(length(unique(iris$Petal.Length)), 2L))

  test <- FDF(mtcars <- datasets::mtcars)
  test2 <- group_by(test, cyl, vs, am)
  expect_s3_class(test$groups, "tbl_df")
  expect_equal(dim(test$groups), c(1, 1))
  groups_save <- mutate(mtcars, id = 1:n() - 1L) %>%
    group_by(cyl, vs, am) %>%
    dplyr::summarise(ind_row = list(id)) %>%
    dplyr::ungroup()
  expect_identical(test2$groups, groups_save)
  expect_identical(test %>%
                     group_by(cyl, add = TRUE) %>%
                     group_by(vs, add = TRUE) %>%
                     group_by(am, add = TRUE) %>%
                     .$groups, groups_save)

  expect_s4_class(dplyr::group_by(test, cyl, vs, am), "FDF")
})

################################################################################

test_that("all verbs works with group_by()", {

  test <- FDF(datasets::iris) %>% filter_int(sample(150))
  test2 <- group_by(test, Species)
  grouped_iris <- as_tibble(test2)
  # as_tibble() also exports groups (version {dplyr})
  expect_identical(grouped_iris, group_by(as_tibble(test), Species))
  # pull() doesn't care about groups
  expect_identical(pull(test2, Species), pull(grouped_iris, Species))
  # summarize() computes by groups
  expect_identical(summarise(test2, mean(Petal.Length)),
                   summarise(grouped_iris, mean(Petal.Length)))
  # mutate()
  # filter()
  # arrange()
})

################################################################################
