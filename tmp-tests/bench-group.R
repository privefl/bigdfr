iris <- datasets::iris[rep(1:150, 10000), ]

microbenchmark::microbenchmark(
  dplyr::group_by(iris, Sepal.Length),
  order(iris$Sepal.Length),
  data.table::fsort(iris$Sepal.Length),
  unique(iris$Sepal.Length),
  dplyr::group_by(iris, Species),
  split(seq_along(iris$Species), iris$Species),
  order(iris$Species),
  data.table::fsort(iris$Species),
  unique(iris$Species)
)

