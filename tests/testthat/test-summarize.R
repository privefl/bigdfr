context("test-summarize.R")

test_that("summarize() works", {

  test <- FDF(iris <- as_tibble(datasets::iris))

  expect_s3_class(summarize(test), "tbl_df")
  expect_equal(ncol(summarize(test)), 0)

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
})
