

fct <- rep(iris$Species, 1000)
chr <- as.character(fct)
microbenchmark::microbenchmark(
  levels(fct), unique(fct), unique(chr)
)

tmp <- setNames(5:7, unique(chr))
tmp[chr]

u_fct <- unique(fct)
u_chr <- unique(chr)

microbenchmark::microbenchmark(
  match(chr, u_chr),
  match(fct, u_fct),
  as.integer(fct)
)
identical(match(chr, u_chr), as.integer(fct))

# Use case
(strings <- c("toto", "tata", levels(iris$Species)[-1], "tutu"))

new_fct <- rep(iris$Species, 1000)
u_fct <- levels(new_fct)
L <- length(strings)
matches <- match(u_fct, strings)
for (i in which(is.na(matches))) {
  strings[L <- L + 1] <- u_fct[i]
  matches[i] <- L
}
strings
matches

microbenchmark::microbenchmark(
  match(new_fct, strings),  # for strings
  matches[new_fct],         # for fcts
  setNames(matches, nm = u_fct)[new_fct]
)


(strings <- c("toto", "tata", levels(iris$Species)[-1], "tutu"))

new_chr <- as.character(fct)
u_chr <- unique(new_chr)
L <- length(strings)
matches <- match(u_chr, strings)
for (i in which(is.na(matches))) {
  strings[L <- L + 1] <- u_chr[i]
  matches[i] <- L
}
strings
matches

microbenchmark::microbenchmark(
  match(new_chr, strings),  # for strings
  matches[new_chr],         # for fcts uniquely
  setNames(matches, nm = u_chr)[new_chr]
)
