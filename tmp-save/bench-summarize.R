# devtools::install_github("privefl/bigdfr")
library(bigdfr)

# Create a temporary file of ~349 MB (just as an example)
csv <- bigreadr::fwrite2(iris[rep(seq_len(nrow(iris)), 1e5), ],
                         tempfile(fileext = ".csv"))
format(file.size(csv), big.mark = ",")

# Read the csv file in FDF format
(X <- FDF_read(csv))

Xg <- group_by(X, Petal.Length) ## 515 MB / 1000 ms

profvis::profvis({summarize(Xg, mean = mean(Sepal.Length))})
# All: 171 MB // 2.1 sec ; extract_dbl: 133 MB // 1.8 sec
# profvis::profvis({summarize(Xg, mean = mean(Sepal.Length), names = "Sepal.Length")})
# With keep names: 336 MB // 860 Ms ; extract_dbl: 0 MB // 100 ms
# Now: 114 MB // 270 ms

X2 <- as_tibble(X)
Xg2 <- group_by(X2, Petal.Length)
microbenchmark::microbenchmark(
  BIGDFR = tmp1 <- summarize(Xg, mean = mean(Sepal.Length)),
  DPLYR =  tmp2 <- summarize(Xg2, mean = mean(Sepal.Length)),
  times = 10
)
# Unit: milliseconds
#   expr      min       lq     mean   median       uq      max neval
# BIGDFR 222.3938 236.0009 252.7645 258.6748 263.6303 284.7591    10
#  DPLYR 363.1928 398.7916 475.8604 450.2688 582.0178 618.3828    10
identical(tmp1, tmp2)
