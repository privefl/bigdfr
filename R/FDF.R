################################################################################

AUTHORIZED_TYPES <- c("numeric" = 8L, "integer" = 4L, "character" = 2L)

ERROR_TYPE <- "Some column types are not authorized."

FIELDS_TO_COPY <- c("extptr", "nrow_all", "types", "backingfile",
                    "ind_row", "ind_col", "strings", "nstr")

################################################################################

types_after_verif <- function(df) {

  assert_class(df, "data.frame")
  assert_pos(nrow(df))
  assert_pos(ncol(df))

  coltypes <- sapply(df, class)
  coltypes[coltypes == "factor"] <- "character"
  if (!all(coltypes %in% names(AUTHORIZED_TYPES)))
    stop2(ERROR_TYPE)

  AUTHORIZED_TYPES[coltypes]
}

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
    ind_nomatch <- which(is.na(matches))
    if (L + length(ind_nomatch) > 2^16)
      stop2("Can't have more than %s different strings.", 2^16)
    for (i in ind_nomatch) {
      matches[i] <- L
      self$strings[L <- L + 1L] <- u_fct[i]
    }
    self$nstr <- L
    fill_ushort(self$address, j, matches[vec])
  } else {
    u_chr <- unique(vec)
    L <- self$nstr
    matches <- match(u_chr, self$strings)
    ind_nomatch <- which(is.na(matches))
    if (L + length(ind_nomatch) > 2^16)
      stop2("Can't have more than %s different strings.", 2^16)
    for (i in ind_nomatch) {
      self$strings[L <- L + 1L] <- u_chr[i]
    }
    self$nstr <- L
    fill_ushort(self$address, j, match(vec, self$strings) - 1L)
  }
}

################################################################################

#' Class FDF
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
#'   - `$ind_row`: Indices of rows for a subview of the FDF
#'   - `$ind_col`: Indices of columns for a subview view of the FDF
#'   - `$strings`: 2^16 strings to match with integers (between 0 and 65535)
#'   - `$nstr`: Number of unique strings already matched with an integer
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
    nrow_all    = "integer",
    types       = "integer",
    backingfile = "character",
    rds         = "character",
    ind_row     = "integer",
    ind_col     = "integer",
    strings     = "character",
    nstr        = "integer",

    #### Active bindings
    address = function() {
      if (identical(.self$extptr, methods::new("externalptr")))  ## nil pointer
        .self$init_address()

      .self$extptr
    },

    nrow = function() length(.self$ind_row),
    ncol = function() length(.self$ind_col),

    is_saved = function() file.exists(.self$rds)
  ),

  methods = list(

    initialize = function(df_or_FDF, backingfile) {

      if (inherits(df_or_FDF, "FDF")) {  ## COPY FROM A FDF

        fdf <- df_or_FDF
        for (field in FIELDS_TO_COPY) {
          .self[[field]] <- fdf[[field]]
        }

      } else {                           ## INIT FROM A DF

        df <- df_or_FDF

        .self$types       <- types_after_verif(df)
        .self$backingfile <- create_file(backingfile)
        .self$nrow_all    <- nrow(df)
        .self$ind_row     <- rows_along(df)
        .self$ind_col     <- stats::setNames(cols_along(df), names(df))
        .self$strings     <- rep(NA_character_, 2^16)
        .self$nstr        <- 0L

        ## Add columns and fill them with data
        add_bytes(.self$backingfile, .self$nrow_all * sum(.self$types))
        for (j in cols_along(df)) {
          transform_and_fill(.self, df, j)
        }
      }

      .self$rds <- ""
      .self
    },

    copy = function() {
      methods::new(Class = "FDF", df_or_FDF = .self)
    },

    add_columns = function(df) {

      types_before <- .self$types
      types_to_add <- types_after_verif(df)
      keep <-

      .copy <- .self$copy()
      .copy$types   <- c(types_before, types_to_add)
      .copy$ind_col <- c(
        .self$ind_col[ !(names(.self$ind_col) %in% names(df)) ],
        stats::setNames(length(types_before) + cols_along(df), names(df))
      )

      ## Add columns and fill them with data
      add_bytes(.self$backingfile, .self$nrow_all * sum(types_to_add))
      for (j in cols_along(df)) {
        transform_and_fill(.copy, df, j)
      }

      .copy$init_address()
    },

    save = function(rds) {
      assert_ext(rds, "rds")
      saveRDS(.self, path.expand(rds))
      .self$rds <- normalizePath(rds)
      .self
    },

    init_address = function() {
      .self$extptr <- getXPtrFDF(.self$backingfile,
                                 .self$nrow,
                                 .self$ind_row,
                                 .self$types)
      .self
    },

    as_env = function(parent = parent.frame()) {
      name_inds <- names(inds <- .self$ind_col)
      e <- new.env(parent = parent, size = length(inds) + 10L)
      for (i in inds) {
        delayedAssign(name_inds[i], pull(.self, i), assign.env = e)
      }
      e
    },

    show = function() {
      cat(sprintf(
        "A Filebacked Data Frame with %s rows and %s columns.\n",
        .self$nrow, .self$ncol))
      invisible(.self)
    }
  )
)
FDF_RC$lock("nrow_all")

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
  methods::new(Class = "FDF", df_or_FDF = df, backingfile = backingfile)
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

#' Dimension and type methods for class `FDF`.
#'
#' @param x A [FDF][FDF-class].
#'
#' @rdname FDF-methods
#' @export
setMethod("dim",    signature(x = "FDF"), function(x) c(x$nrow, x$ncol))

#' @rdname FDF-methods
#' @export
setMethod("length", signature(x = "FDF"), function(x) x$ncol)

################################################################################
