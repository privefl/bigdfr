library(bigdfr)
test <- FDF(datasets::iris) %>% filter_int(sample(150))
test2 <- group_by(test, Species)
grouped_iris <- as_tibble(test2)

str(grouped_iris)
grouped_iris %>%
  arrange(Sepal.Width, .by_group = FALSE)


test2$ind_row
ord <- order(pull(test, "Sepal.Width"))
new <- test2$ind_row[ord]
match(test2$ind_row, ord)
bigdfr:::extract_dbl(test2$address, 2, list(new))[[1]]
indices <- test2$groups$ind_row
ind <- indices[[1]]
true <- order(bigdfr:::extract_dbl(test2$address, 2, indices)[[1]])
bigdfr:::extract_dbl(test2$address, 2, list(ind[true]))
bigdfr:::extract_ushort(test2$address, 5, list(ind[true]))

bigdfr:::extract_ushort(test2$address, 5, indices)
bigdfr:::extract_dbl(test2$address, 2, indices)
tmp <- lapply(indices, function(ind) {
  ind[order(match(ind, new))]
})
print(tmp)
bigdfr:::extract_ushort(test2$address, 5, tmp)
bigdfr:::extract_dbl(test2$address, 2, tmp)
match


match(ind, test2$ind_row)
test2$ind_row[match(ind, test2$ind_row)] == ind
match(match(ind, test2$ind_row), ord)
match(test2$ind_row, new)
