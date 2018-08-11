################################################################################

transform_chr <- function(self, df_j) {
  u_chr <- unique(df_j)
  L <- self$nstr
  matches <- match(u_chr, self$strings)
  ind_nomatch <- which(is.na(matches))
  if (L + length(ind_nomatch) > NSTR_MAX)
    stop2("Can't have more than %s different strings.", NSTR_MAX)
  for (i in ind_nomatch) {
    self$strings[L <- L + 1L] <- u_chr[i]
  }
  self$nstr <- L
  match(df_j, self$strings) - 1L
}

################################################################################

transform_fct <- function(self, df_j) {
  u_fct <- levels(df_j)
  L <- self$nstr
  matches <- match(u_fct, self$strings) - 1L
  ind_nomatch <- which(is.na(matches))
  if (L + length(ind_nomatch) > NSTR_MAX)
    stop2("Can't have more than %s different strings.", NSTR_MAX)
  for (i in ind_nomatch) {
    matches[i] <- L
    self$strings[L <- L + 1L] <- u_fct[i]
  }
  self$nstr <- L
  NA_replace(matches[df_j])
}

################################################################################

fill_transformed <- function(self, df_j, j2) {

  switch(class(df_j),
         numeric   = fill_double(self$address, j2, df_j),
         integer   = fill_int   (self$address, j2, df_j),
         character = fill_ushort(self$address, j2, transform_chr(self, df_j)),
         factor    = fill_ushort(self$address, j2, transform_fct(self, df_j)),
         stop2(ERROR_TYPE))
}

################################################################################
