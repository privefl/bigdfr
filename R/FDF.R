################################################################################

AUTHORIZED_TYPES <- c("numeric" = 8L, "integer" = 4L, "character" = 2L)

ERROR_TYPE <- "Some column types are not authorized."

FIELDS_TO_COPY <- c("extptr", "nrow_all", "types", "backingfile",
                    "ind_row", "ind_col", "strings", "nstr")

NSTR_MAX <- 2^16

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
#' @include fill-transformed.R
#'
FDF_RC <- methods::setRefClass(

  "FDF",

  fields = list(

    extptr      = "externalptr",
    nrow_all    = "integer",      ## nrow at initialization
    types       = "integer",      ## global types of ALL columns
    backingfile = "character",
    rds         = "character",
    ind_row     = "integer",      ## global indices to access
    ind_col     = "integer",      ## global indices to access in order of names
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
    colnames = function() stats::setNames(nm = names(.self$ind_col)),

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
        .self$strings     <- rep(NA_character_, NSTR_MAX)
        .self$nstr        <- 1L   ## Always include NA_character_ first

        ## Add columns and fill them with data
        add_bytes(.self$backingfile, .self$nrow_all * sum(.self$types))
        for (j in cols_along(df)) {
          fill_transformed(.self, df[[j]], j)
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
      new_ind_col  <- length(types_before) + cols_along(df)

      .copy <- .self$copy()
      .copy$types   <- c(types_before, types_to_add)
      .copy$ind_col <- c(
        .self$ind_col[ !(names(.self$ind_col) %in% names(df)) ],
        stats::setNames(new_ind_col, names(df))
      )

      ## Add columns and fill them with data
      add_bytes(.self$backingfile, .self$nrow_all * sum(types_to_add))
      .copy$init_address()
      for (j in cols_along(df)) {
        fill_transformed(.copy, df[[j]], new_ind_col[j])
      }

      .copy
    },

    save = function(rds) {
      assert_ext(rds, "rds")
      saveRDS(.self, path.expand(rds))
      .self$rds <- normalizePath(rds)
      .self
    },

    ## Need this when modifying $ind_row or $types
    init_address = function() {
      .self$extptr <- getXPtrFDF(.self$backingfile,
                                 .self$nrow_all,
                                 .self$ind_row,
                                 .self$types)
      .self
    },

    ## Need this as data mask
    as_env = function(parent = parent.frame()) {
      name_inds <- names(inds <- .self$ind_col)
      e <- new.env(parent = parent, size = length(inds) + 10L)
      for (i in inds) {
        delayedAssign(name_inds[i], pull(.self, i), assign.env = e)
      }
      delayedAssign("ERROR_DELAYED", stop("ERROR NOT DELAYED"), assign.env = e)
      e
    },

    ## When printing
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
