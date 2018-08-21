[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.org/privefl/bigdfr.svg?branch=master)](https://travis-ci.org/privefl/bigdfr)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/privefl/bigdfr?branch=master&svg=true)](https://ci.appveyor.com/project/privefl/bigdfr)
[![Coverage status](https://codecov.io/gh/privefl/bigdfr/branch/master/graph/badge.svg)](https://codecov.io/github/privefl/bigdfr?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/bigdfr)](https://cran.r-project.org/package=bigdfr)

# R package {bigdfr}

R package to operate with data frames stored on disk

[LIST OF FUNCTIONS](https://privefl.github.io/bigdfr/reference/index.html)

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

# An other way to get a tibble is to use summarize()
X %>%
  group_by(Species) %>%
  summarize(min_length = min(Sepal.Length))
```

## How does it work?

I use a binary file on disk to store variables. Operations like `mutate` grow the file to add new columns. Operation like `subset`, `filter` and `arrange` just use indices to access a subset of the file. When (and only when) some columns are needed for some computations, data are actually accessed in memory.


## Differences with {dplyr}

- In `group_by`, variables are passed the same way as in `select`. If you want to use temporary variables, use `mutate`.
- This is allowed to `summarize` data with a function that returns a value of length > 1 (you'll get a list-column).
- When adding columns to an FDF (e.g. with `mutate`), these columns always go last even if they existed before. This means that you can do `FDF(iris) %>% mutate(Sepal.Width = Sepal.Width + 10) %>% pull()` to get the newly created "Sepal.Width" variable.
- You can't have list-columns stored in a FDF.


## TODO

1. optimize when possible
1. rethink `group_by`
1. implement `n()`
1. parallelize some `lapply` with {future}?
1. user-defined summarize on all groups at once?
1. implement fresh backingfile? (when subview is too small -> just use `as_tibble()`?)
1. support dates
1. ...
