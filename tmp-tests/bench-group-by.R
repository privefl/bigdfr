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

species <- as.factor(X2$Species)

bench::mark(
  BIGDFR = group_by(X, Species),
  DPLYR =  group_by(X2, Species),
  SPLIT = split(seq_along(species), species),
  iterations = 10,
  check = FALSE
)[1:10]

microbenchmark::microbenchmark(
  BIGDFR = group_by(X, Species),
  DPLYR =  group_by(X2, Species),
  SPLIT = split(seq_along(species), species),
  times = 10
)
## Unoptimized
# Unit: milliseconds
#   expr      min       lq     mean   median       uq      max neval
# BIGDFR 719.9621 826.5299 851.9569 878.5372 899.5239 906.0895    10
#  DPLYR 513.6052 545.7875 590.1804 592.0674 634.8092 646.3368    10
#  SPLIT 176.8372 178.4274 225.4691 200.5573 238.1912 370.1223    10

## Optimized
# Unit: milliseconds
#   expr      min       lq     mean   median       uq      max neval
# BIGDFR 395.4105 404.9959 484.6515 450.1472 615.0284 649.2184    10
#  DPLYR 434.1219 443.9586 455.5181 451.2141 471.8601 484.0129    10
#  SPLIT 176.8536 177.9356 220.8731 206.6348 249.4011 299.6146    10

profvis::profvis(group_by(X,  Species)) ## 450 MB / 570 ms
profvis::profvis(group_by(X2, Species)) ##  57 MB / 470 ms



microbenchmark::microbenchmark(
  BIGDFR = group_by(X, Petal.Length),
  DPLYR =  group_by(X2, Petal.Length),
  times = 10
)
# Unit: milliseconds
#   expr      min       lq     mean    median        uq      max neval
# BIGDFR 795.4027 798.9297 982.5882 1008.7556 1042.5664 1267.032    10
#  DPLYR 386.6005 392.7857 430.2401  403.8572  462.1967  516.301    10

profvis::profvis(Xg <- group_by(X,  Petal.Length)) ## 515 MB / 1000 ms
profvis::profvis(Xg2 <- group_by(X2, Petal.Length)) ##  50 MB /  450 ms

microbenchmark::microbenchmark(
  BIGDFR = summarize(Xg, mean = mean(Sepal.Length)),
  DPLYR =  summarize(Xg2, mean = mean(Sepal.Length)),
  times = 10
)
# Unit: milliseconds
#   expr      min        lq      mean   median        uq       max neval
# BIGDFR 2048.818 2052.0113 2138.2401 2111.184 2203.7107 2306.8719    10
#  DPLYR  289.145  290.2044  295.5219  293.894  297.5263  307.7652    10

profvis::profvis({summarize(Xg, mean = mean(Sepal.Length), names = "Sepal.Length")})
# All: 171 MB // 2.1 sec ; extract_dbl: 133 MB // 1.8 sec


system.time(X3 <- mutate(X, bool = Species == "setosa"))
# utilisateur     système      écoulé
#       0.242       0.076       0.318
system.time(X4 <- mutate(X2, bool = Species == "setosa"))
# utilisateur     système      écoulé
#       0.065       0.000       0.066
