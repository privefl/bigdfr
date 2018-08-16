.data <- FDF(iris) %>% group_by(Species)
list_ind_row <- .data$groups$ind_row
names_pulled <- lapply(rlang::set_names(c("Petal.Length")), function(var_name) {
  bigdfr:::extract_var(.data, var_name, list_ind_row)
})
str(names_pulled)

tmp <- lapply(seq_along(list_ind_row), function(k) {
  names_pulled_group_k <- lapply(names_pulled, function(x) x[[k]])
  grouped_df <- dplyr::group_by_at(as_tibble(names_pulled_group_k),
                                   names(names_pulled_group_k))
  attributes(grouped_df)[c("labels", "indices")]
})
str(tmp)

list_list_ind <- lapply(tmp, function(x) x$indices)
sizes <- sapply(list_list_ind, length)
list_ind <- unlist(list_list_ind, recursive = FALSE)
rel_to_abs(list_ind_row, list_ind, sizes)

all_labels <- do.call(dplyr::bind_rows, lapply(tmp, function(x) x$labels))
rep_prev <- .data$groups[rep(seq_along(sizes), sizes), ]
dim(all_labels)
dim(rep_prev)

