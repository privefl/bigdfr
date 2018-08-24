# devtools::install_github("privefl/bigdfr")
library(bigdfr)

# Create a temporary file of ~349 MB (just as an example)
csv <- bigreadr::fwrite2(iris[rep(seq_len(nrow(iris)), 1e5), ],
                         tempfile(fileext = ".csv"))
format(file.size(csv), big.mark = ",")

# Read the csv file in FDF format
(X <- FDF_read(csv))

X2 <- as_tibble(X)
object.size(X2)

microbenchmark::microbenchmark(
  BIGDFR = X <- mutate(X, bool = Species == "setosa"),
  DPLYR = mutate(X2, bool = Species == "setosa"),
  times = 20
)
# Unit: milliseconds
#   expr       min        lq     mean    median       uq      max neval
# BIGDFR 338.16397 367.65338 444.9147 426.62671 498.4371 770.3622    20
#  DPLYR  77.70228  79.59503 103.0608  81.85201 113.2372 274.4219    20

profvis::profvis(X <- mutate(X, bool = Species == "setosa"))

# bench::mark(
#   BIGDFR = X <- mutate(X, bool = Species == "setosa"),
#   DPLYR = mutate(X2, bool = Species == "setosa"),
#   iterations = 20,
#   check = FALSE
# )
# expression     min    mean median      max `itr/sec` mem_alloc  n_gc
# <chr>      <bch:t> <bch:t> <bch:> <bch:tm>     <dbl> <bch:byt> <dbl>
# BIGDFR     381.9ms   732ms  529ms    1.74s      1.37        0B     7
# DPLYR       74.5ms  99.6ms  112ms 130.26ms     10.0         0B     1

