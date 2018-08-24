################################################################################

transform_chr <- function(self, df_j, j2) {
  u_chr <- unique_chr(df_j)
  L <- self$nstr
  matches <- match_chr(u_chr, self$strings)
  ind_nomatch <- which(is.na(matches))
  if (L + length(ind_nomatch) > NSTR_MAX)
    stop2("Can't have more than %s different strings.", NSTR_MAX)
  for (i in ind_nomatch) {
    self$strings[L <- L + 1L] <- u_chr[i]
  }
  self$nstr <- L
  self$meta[[j2]] <- append(self$meta[j2][[1]], list(uniq = u_chr))
  match_chr(df_j, self$strings)
}

################################################################################

transform_fct <- function(self, df_j) {
  u_fct <- levels(df_j)
  L <- self$nstr
  matches <- match_chr(u_fct, self$strings) - 1L
  ind_nomatch <- which(is.na(matches))
  if (L + length(ind_nomatch) > NSTR_MAX)
    stop2("Can't have more than %s different strings.", NSTR_MAX)
  for (i in ind_nomatch) {
    matches[i] <- L
    self$strings[L <- L + 1L] <- u_fct[i]
  }
  self$nstr <- L
  matches
}

################################################################################

fill_transformed <- function(self, df_j, j2) {

  # TODO: change this
  if (length(df_j) == 1) df_j <- rep(df_j, self$nrow)

  self$meta[[j2]] <- attributes(df_j)
  switch(class2(df_j),
         Date      = ,
         POSIXt    = ,
         numeric   = fill_dbl(self, j2, df_j),
         integer   = fill_int(self, j2, df_j),
         logical   = fill_lgl(self, j2, df_j),
         factor    = fill_fct(self, j2, df_j, transform_fct(self, df_j)),
         character = fill_chr(self, j2, transform_chr(self, df_j, j2)),
         stop2(ERROR_TYPE))
}

################################################################################
