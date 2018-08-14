################################################################################

context("test-summarize.R")

################################################################################

test_that("slapply() works", {
  test1 <- slapply(1:4, function(i) rnorm(i))
  expect_true(is.list(test1))
  expect_equal(lengths(test1), 1:4)
  test2 <- slapply(1:4, function(i) rnorm(1, i))
  expect_false(is.list(test2))
  expect_length(test2, 4)
  test3 <- slapply(1:4, function(i) list(rnorm(1, i)))
  expect_true(is.list(test3))
  expect_equal(lengths(test3), rep(1, 4))
})

################################################################################

test_that("summarize() works", {

  test <- FDF(iris <- as_tibble(datasets::iris))

  for (fun in list(summarize, summarise, dplyr::summarise, dplyr::summarize)) {
    expect_s3_class(summarize(test), "tbl_df")
    expect_equal(ncol(summarize(test)), 0)
  }

  expect_identical(summarize(iris, Species = 150, Species = Species + 1),
                   summarize(test, Species = 150, Species = Species + 1))

  expect_identical(test %>% group_by(Sepal.Width) %>% summarize(),
                   iris %>% group_by(Sepal.Width) %>% summarize())

  n_distinct <- function(x) length(unique(x))

  expect_identical(summarize(test, Species = n_distinct(Species),
                             mean_length = mean(Sepal.Length)),
                   summarize(iris, Species = n_distinct(Species),
                             mean_length = mean(Sepal.Length)))

  expect_identical(test %>%
                     group_by(Sepal.Width) %>%
                     summarize(Species = n_distinct(Species),
                               mean_length = mean(Sepal.Length)),
                   iris %>%
                     group_by(Sepal.Width) %>%
                     summarize(Species = n_distinct(Species),
                               mean_length = mean(Sepal.Length)))

  test2 <- test %>%
    group_by(Species) %>%
    summarize(range = range(Sepal.Length)) %>%
    pull(range)
  expect_true(is.list(test2))
  expect_equal(lengths(test2), rep(2, 3))
})

################################################################################
