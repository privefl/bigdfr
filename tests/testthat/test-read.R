################################################################################

context("test-read.R")

################################################################################

M <- 500

csv <- tempfile(fileext = ".csv")

SEPS <- c(" ", ",", ";", "|", "\t")

################################################################################

test_that("read with or without dimnames", {

  for (header in c(FALSE, TRUE)) {

    for (t in c("integer", "double")) {

      N <- nrow(mtcars)
      x <- matrix(rnorm(N * M, mean = 100, sd = 5), N, M)
      storage.mode(x) <- t

      for (sep in SEPS) {

        df <- cbind.data.frame(mtcars, x)
        bigreadr::fwrite2(df, csv, sep = sep, col.names = header)

        expect_error(FDF_read(csv, select = rev(seq_len(M))),
                     "Argument 'select' should be sorted.", fixed = TRUE)


        skip <- sample(0:2, 1)
        skip2 <- max(0, skip - header)
        ind_no_skip <- setdiff(seq_len(N), seq_len(skip2))
        header2 <- header && (skip == 0)

        tmp <- tempfile()
        test <- FDF_read(csv, select = 5:50, skip = skip, backingfile = tmp,
                         nb_parts = sample(5, 1))
        df_part <- df[5:50]

        expect_true(all(tail(test$types, 30) == `if`(t == "integer", 4L, 8L)))
        expect_equal(test$backingfile, normalizePath(tmp))
        expect_false(test$is_saved)
        expect_equal(as.matrix(as_tibble(select(test, 8:46))),
                     as.matrix(as_tibble(df_part[ind_no_skip, 8:46])),
                     check.attributes = FALSE)
      }

    }

  }
})

################################################################################

test_that("read with filtering", {

  for (t in c("integer", "double")) {

    N <- nrow(mtcars)
    x <- matrix(rnorm(N * M, mean = 100, sd = 5), N, M)
    storage.mode(x) <- t

    filter0 <- (mtcars$cyl == 4)

    for (sep in SEPS) {

      filter <- sample(list(filter0, which(filter0), -which(!filter0)))[[1]]

      df <- cbind.data.frame(mtcars, x)
      bigreadr::fwrite2(df, csv, sep = sep, col.names = TRUE)

      tmp <- tempfile()
      test <- FDF_read(csv, select = 5:50, backingfile = tmp, filter = filter)
      df_part <- df[filter, 5:50]

      expect_true(all(tail(test$types, 30) == `if`(t == "integer", 4L, 8L)))
      expect_equal(test$backingfile, normalizePath(tmp))
      expect_false(test$is_saved)
      expect_equal(as.matrix(as_tibble(select(test, 8:46))),
                   as.matrix(as_tibble(df_part[8:46])),
                   check.attributes = FALSE)
    }

  }

})

################################################################################
