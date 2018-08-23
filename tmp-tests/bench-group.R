iris <- datasets::iris[rep(1:150, 10000), ]

y2 <- as.character(iris$Species)
iris$y2 <- y2

microbenchmark::microbenchmark(
  DPLYR1 = dplyr::group_by(iris, Sepal.Length),
  ORDER1 = order(iris$Sepal.Length),
  DT1 = data.table::fsort(iris$Sepal.Length),
  UNIQ1 = unique(iris$Sepal.Length),
  DPLYR2 = dplyr::group_by(iris, Species),
  SPLIT2 = split(seq_along(iris$Species), iris$Species),
  ORDER2 = order(iris$Species),
  UNIQ2 = unique(iris$Species),
  DPLYR3 = dplyr::group_by(iris, y2),
  SPLIT3 = split(seq_along(y2), y2),
  ORDER3 = order(y2),
  UNIQ3 = unique(y2)
)
# Unit: milliseconds
#   expr          min           lq        mean       median           uq         max neval    cld
# DPLYR1    24.293187    25.221059    28.80940    26.154559    26.931952    84.75268   100 a c
# ORDER1    78.092196    79.912679    86.58341    80.822341    82.104973   137.07338   100     e
#    DT1    15.740544    16.152913    30.20878    16.319119    16.831145   314.87214   100  bc
#  UNIQ1    23.048299    24.051991    39.62547    24.470153    73.853122    90.77945   100   cd
# DPLYR2     7.625599     8.028863    12.42759     8.339589     8.611411    65.60227   100 a
# SPLIT2     8.539731     9.489289    17.45606     9.685457     9.929303    67.06567   100 ab
# ORDER2     6.299264     8.518871    19.22311     8.746992     8.948126   299.74874   100 ab
#  UNIQ2    10.717621    11.085128    17.67445    11.249513    11.639533   315.07973   100 ab
# DPLYR3    43.266135    44.601409    51.77880    45.269708    46.626339   103.67100   100    d
# SPLIT3    41.348478    43.117311    76.29974    91.970541    95.123484   337.92143   100     e
# ORDER3 11605.322954 11630.235936 11702.26922 11695.482780 11751.841424 12148.20155   100      f
#  UNIQ3    17.379426    17.684357    26.95210    17.993922    18.473006    76.26228   100 a c
