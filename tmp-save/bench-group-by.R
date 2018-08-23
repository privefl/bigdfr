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

profvis::profvis(FDF(X2))

species <- as.factor(X2$Species)

# bench::mark(
#   BIGDFR = group_by(X, Species),
#   DPLYR =  group_by(X2, Species),
#   SPLIT = split(seq_along(species), species),
#   iterations = 10,
#   check = FALSE
# )[1:10]

microbenchmark::microbenchmark(
  BIGDFR = group_by(X, Species),
  DPLYR =  group_by(X2, Species),
  # HYBRID = group_by(tibble(Species = pull(X, Species)), Species),
  SPLIT = split(seq_along(species), species),
  times = 10
)
## Unoptimized
# Unit: milliseconds
#   expr      min       lq     mean   median       uq      max neval
# BIGDFR 719.9621 826.5299 851.9569 878.5372 899.5239 906.0895    10
#  DPLYR 513.6052 545.7875 590.1804 592.0674 634.8092 646.3368    10
#  SPLIT 176.8372 178.4274 225.4691 200.5573 238.1912 370.1223    10
str(tmp <- group_by(tibble(Species = pull(X, Species)), Species))

## Optimized
# Unit: milliseconds
#   expr      min       lq     mean   median       uq       max neval
# BIGDFR 344.9019 438.3180 995.9210 530.9372 624.6551 5411.2339    10
#  DPLYR 498.1834 513.4290 522.0560 522.4928 534.2056  536.2535    10
# HYBRID 627.4343 633.0317 744.3715 660.6058 817.9610 1188.7904    10
#  SPLIT 215.1617 258.2040 297.9788 278.3871 316.8386  443.4897    10

# profvis::profvis(group_by(X,  Species)) ## 450 MB / 570 ms
profvis::profvis(group_by(X2, Species)) ##  57 MB / 470 ms
# profvis::profvis(group_by(tibble(Species = pull(X, Species)), Species))  # 57 MB / 640 ms
profvis::profvis(group_by(X, Species)) ## Now: 57 MB / 680 ms

# tmp <- arrange(as_tibble(test_multimap2(X$address, 1, X$ind_row)), key)

microbenchmark::microbenchmark(
  BIGDFR = group_by(X, Petal.Length),
  # UMMAP = arrange(as_tibble(test_multimap2(X$address, 1, X$ind_row)), key),
  # HYBRID = group_by(tibble(Petal.Length = pull(X, Petal.Length)), Petal.Length),
  DPLYR =  group_by(X2, Petal.Length),
  times = 10
)
# Unit: milliseconds
#   expr      min       lq     mean   median       uq       max neval
# BIGDFR 768.5909 784.6507 858.6700 851.7373 899.0602 1033.0810    10
# HYBRID 465.9900 474.6588 501.0705 491.5518 525.1862  539.6337    10
#  DPLYR 398.8971 405.9700 438.3076 429.9301 461.8193  510.3565    10
# UMMAP: 6 sec :/

# profvis::profvis(Xg <- group_by(X,  Petal.Length)) ## 515 MB / 1000 ms
profvis::profvis(Xg2 <- group_by(X2, Petal.Length)) ##  50 MB /  450 ms
# profvis::profvis(group_by(tibble(Petal.Length = pull(X, Petal.Length)), Petal.Length)) #  44 MB / 550 ms
profvis::profvis(Xg <- group_by(X,  Petal.Length)) ## 57 MB / 640 ms

# tmp <- group_by(tibble(Petal.Length = pull(X, Petal.Length)), Petal.Length)
# tmp$Species <- pull(X, Species)
# tmp2 <- group_by(tmp, Species, add = TRUE)
# tmp$Petal.Length <- NULL
# tmp2 <- group_by(tmp, Species, add = TRUE) # have to keep it in memory -> not possible




microbenchmark::microbenchmark(
  BIGDFR  = group_by(X,  Species, Petal.Length),
  DPLYR   = group_by(X2, Species, Petal.Length),
  BIGDFR2 = group_by(X,  Species) %>% group_by(Petal.Length, add = TRUE),
  DPLYR2  = group_by(X2, Species) %>% group_by(Petal.Length, add = TRUE),
  times = 10
)
# Unit: milliseconds
#    expr       min        lq      mean    median        uq       max neval
#  BIGDFR  853.5683  935.0157  944.6567  955.2310  962.8241 1014.8244    10
#   DPLYR  559.1658  575.5327  593.7990  595.8903  607.2206  629.4433    10
# BIGDFR2 1190.8752 1216.6773 1267.3589 1275.4496 1313.2873 1360.4640    10
#  DPLYR2 1047.4149 1083.8713 1109.7999 1098.0080 1114.7918 1233.8783    10

# TODO: move mutate in its own file
# system.time(X3 <- mutate(X, bool = Species == "setosa"))
# # utilisateur     système      écoulé
# #       0.242       0.076       0.318
# system.time(X4 <- mutate(X2, bool = Species == "setosa"))
# # utilisateur     système      écoulé
# #       0.065       0.000       0.066
