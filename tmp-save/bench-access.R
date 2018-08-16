# devtools::install_github("privefl/bigdfr")
library(bigdfr)

# Create a temporary file of ~349 MB (just as an example)
csv <- bigreadr::fwrite2(iris[rep(seq_len(nrow(iris)), 1e5), ],
                         tempfile(fileext = ".csv"))
format(file.size(csv), big.mark = ",")

# Read the csv file in FDF format
(X <- FDF_read(csv))

X2 <- as_tibble(X)

Xg <- group_by(X,  Petal.Length) ## 515 MB / 1000 ms

system.time(
  tmp <- lapply(Xg$groups$rel_ind_row, function(ind) {
    part <- X2$Sepal.Length[ind]
    mean(part)
  })
) # 0.2 max
all(lengths(tmp) == 1)
unlist(tmp)

system.time(
  tmp2 <- lapply(Xg$groups$rel_ind_row, function(ind) {
    copy <- X$copy(ind_row = X$ind_row[ind])
    part <- pull(copy, 1)
    mean(part)
  })
) # 2 sec

identical(tmp, tmp2)

system.time(all <- pull(X, 1))

list_inds <- lapply(Xg$groups$rel_ind_row, function(ind) ind - 1L)

system.time({
  list_pull <- bigdfr:::extract_var(X, "Sepal.Length", list_inds)
  tmp3 <- lapply(list_pull, mean)
}) # 0.2 sec

identical(tmp, tmp3)
