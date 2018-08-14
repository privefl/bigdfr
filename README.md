[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.org/privefl/bigdfr.svg?branch=master)](https://travis-ci.org/privefl/bigdfr)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/privefl/bigdfr?branch=master&svg=true)](https://ci.appveyor.com/project/privefl/bigdfr)
[![Coverage status](https://codecov.io/gh/privefl/bigdfr/branch/master/graph/badge.svg)](https://codecov.io/github/privefl/bigdfr?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/bigdfr)](https://cran.r-project.org/package=bigdfr)

# R package {bigdfr}

R package to operate with data frames stored on disk

## Example

```r
# devtools::install_github("privefl/bigdfr")
library(bigdfr)

# Create a temporary file of ~349 MB (just as an example)
csv <- bigreadr::fwrite2(iris[rep(seq_len(nrow(iris)), 1e5), ], 
                         tempfile(fileext = ".csv"))
format(file.size(csv), big.mark = ",")

# Read the csv file in FDF format
(X <- FDF_read(csv))
head(X)
file.size(X$backingfile)
X$types

# Standard {dplyr} operations
X2 <- X %>% 
  filter(Species == "virginica", Sepal.Length < 5) %>%
  mutate(Sepal.Length = Sepal.Length + 1) %>%
  arrange(desc(Sepal.Length))
  
# Export as tibble (fully in memory, e.g. after sufficient filtering)
as_tibble(X2)
```

## Differences with {dplyr}

- In `group_by`, variables are passed the same way as in `select`. If you want to use temporary variables, use `mutate`.
- This is allowed to summarize data with a function that returns a value of length > 1.
