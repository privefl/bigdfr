################################################################################

context("test-n.R")

################################################################################

test_that("find_n() works", {
  e <- environment()
  expect_false(find_n(e))
  n <- 1:5
  e <- environment()
  expect_false(find_n(e))
  n <- function() 2
  e <- environment()
  expect_true(find_n(e))
  expect_error(dplyr::n())
  expect_true(find_n(e))
  rm(n)
  e <- environment()
  expect_false(find_n(e))
})

################################################################################

test_that("repl_call_n() works", {
  to_test <- function(quo, val)
    repl_call_n(rlang::quo_get_expr(rlang::enquo(quo)), val)
  expect_identical(to_test(n(), 52), 52)
  expect_identical(to_test(2 + n(), 52), quote(2 + 52))
  expect_identical(to_test(mean(2 + n()), 52), quote(mean(2 + 52)))
})

################################################################################

test_that("n() works", {

  test <- FDF(iris <- datasets::iris)
  test2 <- mutate(test, n = n())
  expect_identical(pull(test2, n), rep(150L, 150))

  test3 <- group_by(test2, Species)
  iris3 <- as_tibble(test3)
  expect_identical(summarise(test3, n2 = n(), n = n[1]),
                   summarise(iris3, n2 = as.integer(n()), n = n[1]))
  expect_identical(summarise(test3, n = n(), n = n[1]),
                   summarise(iris3, n = as.integer(n()), n = n[1]))

  n <- function() 2
  expect_identical(summarise(test3, n2 = n(), n = n[1]),
                   summarise(iris3, n2 = n(), n = n[1]))
  expect_identical(summarise(test3, n = n(), n = n[1]),
                   summarise(iris3, n = n(), n = n[1]))

  expect_error(group_by(test, Species) %>% summarise(n(), n = n[1]))

  n <- 1:5
  expect_identical(summarise(test3, n2 = n(), n = n[1]),
                   summarise(iris3, n2 = as.integer(n()), n = n[1]))
  expect_identical(summarise(test3, n = n(), n = n[1]),
                   summarise(iris3, n = as.integer(n()), n = n[1]))

  expect_identical(group_by(test, Species) %>% summarise(n(), n = n[1]),
                   group_by(iris, Species) %>% summarise(n(), n = n[1]))
})

################################################################################

test_that("n() works within filter", {
  test <- FDF(iris <- datasets::iris)
  expect_equal(dim(filter(test, n() > 15)), dim(test))
  test2 <- group_by(test, Species, Petal.Length)
  len_grp <- lengths(test2$groups$ind_row)
  test3 <- filter(test2, n() > 5)
  expect_equal(sum(len_grp[len_grp > 5]), nrow(test3))
  expect_identical(names(test3$groups), names(test2$groups))
  expect_equal(as_tibble(test3), filter(as_tibble(test2), n() > 5))
})

################################################################################
