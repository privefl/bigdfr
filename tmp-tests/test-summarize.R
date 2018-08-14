library(dplyr)
library(rlang)
library(magrittr)
library(bigdfr)

Species <- 5
summarize(iris, Species = n(), Species = Species + 1)

test <- FDF(iris)

test_summarize <- function(test, ...) {

  name_dots <- names(dots <- quos(...))
  groups <- test$groups

  for (i in seq_along(dots)) {

    parent_env <- quo_get_env(dots[[i]])

    groups %<>%
      mutate(
        !!sym(name_dots[i]) := sapply(rel_ind_row, function(ind) {
          .copy <- test$copy(ind_row = test$ind_row[ind])
          e <- .copy$as_env(parent = parent_env)
          dots[[i]] %>%
            quo_set_env(as_env(groups, parent = e)) %>%
            eval_tidy()
        })
      )
  }

  select(groups, -rel_ind_row)
}


# Species <- 5

summarize(iris, Species = 150, Species = Species + 1)
test_summarize(test, Species = 150, Species = Species + 1)

# test_summarize(test)
# summarize(as_tibble(iris))
test_summarize(test, Species = n_distinct(Species),
               mean_length = mean(Sepal.Length))


test %>%
  test_summarize()
test %>%
  group_by(Sepal.Width) %>%
  test_summarize()

iris %>%
  group_by(Sepal.Width) %>%
  summarise()

test %>%
  group_by(Sepal.Width) %>%
  test_summarize(Species = n_distinct(Species),
                 mean_length = mean(Sepal.Length))



iris %>%
  group_by(Sepal.Width) %>%
  summarize(Species = n_distinct(Species),
            mean_length = mean(Sepal.Length))

