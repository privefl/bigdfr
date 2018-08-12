library(bigdfr)
test <- FDF(datasets::iris)
test$address
test2 <- test$copy()
test2$extptr
split(test$ind_row, pull(test, Species))



rapply(list(1, list(2, 3)), function(x) x + 1, how = "replace")

df <- mtcars
ind_row <- rows_along(df) - 1L
split1 <- split(ind_row, df$cyl)
str(split1)
df1 <- select(df, -cyl)
split2 <- lapply(split1, function(ind) split(ind, f = df1$vs[ind]))
str(split2)
reshape2::melt(split2)


reshape2::melt(rapply(split2, function(x) mean(x), how = "replace"))

long_vec <- rep(df$cyl, 1000)
microbenchmark::microbenchmark(
  unique(long_vec),
  as.factor(long_vec),
  split(seq_along(long_vec), long_vec)
)


long_vec <- rep(df$cyl, 100000)
profvis::profvis({split(seq_along(long_vec), long_vec)})


group <- function(prev, df, by_name) {

  prev %>%
    mutate(NESTED = lapply(rel_ind_row, function(ind) {
      by <- df[[by_name]][ind + 1L]
      u_by <- unique(by)
      tibble(!!sym(by_name) := u_by,
             rel_ind_row = lapply(u_by, function(x) ind[by == x]))
    })) %>%
    select(-rel_ind_row) %>%
    tidyr::unnest(NESTED)
}

df <- mtcars
ind_row <- rows_along(df) - 1L
(first <- tibble(rel_ind_row = list(seq_along(ind_row))))
(second <- group(first, df, "cyl"))
(third <- group(second, df, "vs"))
(fourth <- group(third, df, "am"))

## Summarize drops ONE group (last one)
mtcars %>%
  group_by(cyl, vs, am) %>%
  summarise(min(mpg))
fourth


mtcars %>%
  group_by(cyl) %>%
  summarise(min(mpg))

second
