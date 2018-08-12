library(bigdfr)
test <- FDF(df <- datasets::mtcars)
test$groups
ind_row <- test$ind_row

(first <- tibble(rel_ind_row = list(seq_along(ind_row))))
(second <- group(first, df, "cyl"))
(third <- group(second, df, "vs"))
(fourth <- group(third, df, "am"))

group <- function(data, name_vars = NULL) {

  ind_row <- data$ind_row
  groups_before <- `if`(nrow(data$groups) == 0,
                        tibble(rel_ind_row = list(seq_along(ind_row))),
                        data$groups)

  for (name in name_vars) {
    groups_before <- groups_before %>%
      mutate(NESTED = lapply(rel_ind_row, function(ind) {
        .part <- data$copy(ind_row = ind_row[ind])
        by <- pull(.part, name)
        u_by <- unique(by)
        tibble(!!sym(name) := u_by,
               rel_ind_row = lapply(u_by, function(x) ind[by == x]))
      })) %>%
      select(-rel_ind_row) %>%
      tidyr::unnest(NESTED)
  }

  data$copy(groups = groups_before)
}

tmp <- group(test, c("cyl", "vs", "am"))$groups
