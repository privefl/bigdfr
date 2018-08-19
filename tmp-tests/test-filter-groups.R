library(dplyr)
flights_sml <- select(nycflights13::flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
)
flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)
iris %>%
  group_by(Species) %>%
  filter(rank(desc(Sepal.Length)) < 10) %>%
  pull(Sepal.Length)

test <- FDF(iris) %>%
  group_by(Species)
list_ind_row <- test$groups$ind_row
pulled <- bigdfr:::extract_dbl(test$address, 1, list_ind_row)
indices <- lapply(seq_along(list_ind_row), function(i) {
  bool <- rank(desc(pulled[[i]])) < 10
  list_ind_row[[i]][bool]
})
groups <- test$groups
groups$ind_row <- indices
bigdfr:::extract_dbl(test$address, 1, indices)
test2 <- test$copy(ind_row = intersect(test$ind_row, unlist(indices)),
                   groups_internal = groups)
test2 %>%
  pull(Sepal.Length)
