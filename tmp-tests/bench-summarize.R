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
Xg2 <- group_by(X2, Petal.Length)

profvis::profvis({summarize(Xg, mean = mean(Sepal.Length), names = "Sepal.Length")})
# All: 171 MB // 2.1 sec ; extract_dbl: 133 MB // 1.8 sec
# With keep names: 336 MB // 860 Ms ; extract_dbl: 0 MB // 100 ms
