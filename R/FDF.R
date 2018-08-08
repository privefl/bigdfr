################################################################################

AUTHORIZED_TYPES <-
  c("numeric" = 8L, "integer" = 4L, "factor" = 2L, "character" = 2L)

ERROR_TYPE <- "Some column types are not authorized."

################################################################################

transform_and_fill <- function(self, df, j) {

  cl <- class(vec <- df[[j]])
  if (cl == "numeric") {
    fill_double(self$address, j, vec)
  } else if (cl == "integer") {
    fill_int(self$address, j, vec)
  } else if (cl == "character") {
    fill_int(self$address, j, vec)
  } else if (cl == "factor") {
    u_fct <- levels(vec)
    L <- self$nstr
    matches <- match(u_fct, self$strings)
    for (i in which(is.na(matches))) {
      self$strings[L <- L + 1L] <- u_fct[i]
      matches[i] <- L
    }
    self$nstr <- L
    fill_ushort(self$address, j, matches[vec])
  } else {
    u_chr <- unique(vec)
    L <- self$nstr
    matches <- match(u_chr, self$strings)
    for (i in which(is.na(matches))) {
      self$strings[L <- L + 1L] <- u_chr[i]
      matches[i] <- L
    }
    self$nstr <- L
    fill_ushort(self$address, j, match(vec, strings))
  }
}

################################################################################

#' Class FBM
#'
#' A reference class for storing and accessing data frames stored on disk.
#'
#' @details
#' An FDF object has many field:
#'   - `$address`: address of the external pointer containing the underlying
#'     C++ object, to be used as a `XPtr<FDF>` in C++ code
#'   - `$extptr`: use `$address` instead
#'   - `$nrow`
#'   - `$ncol`
#'   - `$types`
#'   - `$backingfile`: File that stores the numeric data of the FDF
#'   - `$rds`: 'rds' file (that may not exist) in which this object is stored
#'   - `$is_saved`: whether this object stored in `$rds`?
#'
#' And some methods:
#'   - `$save()`: Save the FDF object in `$rds`. Returns the FDF.
#'
#' @exportClass FDF
#'
FDF_RC <- methods::setRefClass(

  "FDF",

  fields = list(

    extptr      = "externalptr",
    nrow        = "numeric",
    types       = "integer",
    backingfile = "character",
    rds         = "character",
    ind_row     = "integer",
    strings     = "character",
    nstr        = "integer",

    #### Active bindings
    address = function() {
      if (identical(.self$extptr, methods::new("externalptr"))) { # nil
        .self$extptr <- getXPtrFDF(.self$backingfile,
                                   .self$nrow,
                                   .self$ind_row,
                                   .self$types)
      }
      .self$extptr
    },

    ncol = function() length(.self$types),
    is_saved = function() file.exists(.self$rds)
  ),

  methods = list(

    initialize = function(df, backingfile) {

      assert_pos(nrow(df))
      coltypes <- sapply(df, class)
      assert_pos(length(coltypes))
      if (!all(coltypes %in% names(AUTHORIZED_TYPES))) stop2(ERROR_TYPE)

      .self$backingfile <- create_file(backingfile)
      .self$rds         <- ""
      .self$nrow        <- nrow(df)
      .self$ind_row     <- rows_along(df)
      .self$types       <- AUTHORIZED_TYPES[coltypes]
      .self$strings     <- rep(NA_character_, 2^16)
      .self$nstr        <- 0L

      ## Add columns and fill them with data
      add_bytes(.self$backingfile, .self$nrow * sum(.self$types))
      for (j in cols_along(df)) {
        transform_and_fill(.self, df, j)
      }

      .self$address  # connect once
      .self
    },

    add_df = function(df) {

      assert_class(df, "data.frame")

      invisible(.self)
    },

    save = function(rds) {
      assert_ext(rds, "rds")
      saveRDS(.self, path.expand(rds))
      .self$rds <- normalizePath(rds)
      .self
    }
  )
)
FDF_RC$lock("nrow")

################################################################################

#' Wrapper constructor for class `FDF`.
#'
#' @param df A data frame.
#' @param backingfile Path to the file storing the data on disk.
#'   Default stores in the temporary directory.
#'
#' @rdname FDF-class
#'
#' @importFrom methods new
#' @export
#'
FDF <- function(df, backingfile = tempfile()) {
  assert_class(df, "data.frame")
  do.call(methods::new, args = c(Class = "FDF", as.list(environment())))
}

################################################################################
#### Sequence generation ####

#' Sequence generation
#'
#' Similar to [seq_along], it creates sequences of size `nrow(x)` or `ncol(x)`.
#'
#' @param x Any object on which you can call `nrow` and `ncol`.
#'
#' @examples
#' dim(iris)
#' str(rows_along(iris))
#' str(cols_along(iris))
#'
#' @rdname seq-dim
#' @keywords internal
#' @export
rows_along <- function(x) seq_len(nrow(x))

#' @rdname seq-dim
#' @export
cols_along <- function(x) seq_len(ncol(x))

################################################################################
