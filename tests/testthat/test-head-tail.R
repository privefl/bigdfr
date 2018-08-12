context("test-head-tail.R")

test_that("head() and tail() works", {
  for (fun in list(head, tail, utils::head, utils::tail)) {

    test <- FDF(iris <- mutate(datasets::iris, Species = as.character(Species)))
    expect_identical(fun(test), fun(as_tibble(iris)))
    expect_s3_class(fun(test), "tbl_df")
    expect_equal(dim(fun(test, n = 15)), c(15, 5))

    ind_sample <- sample(nrow(iris), sample(c(4, 50), 1))
    test2 <- filter_int(test, ind_sample)
    expect_identical(fun(test2), fun(as_tibble(iris[ind_sample, ])))
  }
})
