################################################################################

AUTHORIZED_TYPES <- c(
  "numeric" = 8L, "Date" = 8L, "POSIXt" = 8L,
  "integer" = 4L, "logical" = 4L,
  "character" = 2L, "factor" = 2L)

ERROR_TYPE <- "Some column types are not authorized."

FIELDS_TO_COPY <- c("nrow_all", "types", "backingfile", "groups_internal",
                    "ind_row", "ind_col", "strings", "nstr", "meta")

NSTR_MAX <- 2^16

################################################################################

class2 <- function(x) {
  tail(class(x), 1)
}

#----

types_after_verif <- function(df) {

  assert_class(df, "data.frame")
  assert_pos(nrow(df))
  assert_pos(ncol(df))

  coltypes <- sapply(df, class2)
  if (!all(coltypes %in% names(AUTHORIZED_TYPES)))
    stop2(ERROR_TYPE)

  AUTHORIZED_TYPES[coltypes]
}

#----

create_file <- function(file) {

  file <- path.expand(file)
  assert_noexist(file)
  assert_dir(dirname(file))
  if (!file.create(file))
    stop2("Problem while create file '%s'.", file)

  normalizePath(file)
}

#----

set_names <- function(x, names = x) {
  stats::setNames(x, names)
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
#'   - `$groups`: Tibble with position indices of `$ind_row` for each group.
#'   - `$groups_internal`: use `$groups` instead
#'   - `$meta`: Meta information for factors, dates and character vectors.
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
    groups_internal = "tbl_df",
    meta        = "list",

    #### Active bindings
    address = function() {
      if (identical(.self$extptr, methods::new("externalptr")))  ## nil pointer
        .self$init_address()

      .self$extptr
    },

    nrow = function() length(.self$ind_row),
    ncol = function() length(.self$ind_col),
    colnames = function() set_names(names(.self$ind_col)),

    is_saved = function() file.exists(.self$rds),
    is_grouped = function() (ncol(.self$groups_internal) > 0),
    groups = function() {
      `if`(.self$is_grouped, .self$groups_internal,
           tibble(ind_row = list(.self$ind_row)))
    }
  ),

  methods = list(

    initialize = function(df_or_FDF, backingfile, ...) {

      if (inherits(df_or_FDF, "FDF")) {            ## COPY FROM A FDF

        name_dots <- names(dots <- list(...))
        for (i in seq_along(dots)) {
          .self[[name_dots[i]]] <- dots[[i]]
        }

        fdf <- df_or_FDF
        for (field in setdiff(FIELDS_TO_COPY, name_dots)) {
          .self[[field]] <- fdf[[field]]
        }

      } else {                                     ## INIT FROM A DF

        assert_nodots()
        df <- df_or_FDF

        .self$types       <- types_after_verif(df)
        .self$backingfile <- create_file(backingfile)
        .self$nrow_all    <- nrow(df)
        .self$ind_row     <- 0L:(nrow(df) - 1L)  ## begin at 0 in C++
        .self$ind_col     <- set_names(cols_along(df), names(df))
        .self$strings     <- rep(NA_character_, NSTR_MAX)
        .self$nstr        <- 1L   ## Always include NA_character_ first
        .self$groups_internal <- tibble()

        ## Add columns and fill them with data
        add_bytes(.self$backingfile, .self$nrow_all * sum(.self$types))
        for (j in cols_along(df)) {
          fill_transformed(.self, df[[j]], j)
        }
      }

      .self$rds <- ""
      .self
    },

    copy = function(...) {
      methods::new(Class = "FDF", df_or_FDF = .self, ...)
    },

    add_columns = function(df) {

      types_before <- .self$types
      if (file.size(.self$backingfile) != .self$nrow_all * sum(types_before))
        stop2("Inconsistency between types and file size.")

      types_to_add <- types_after_verif(df)
      new_ind_col  <- length(types_before) + cols_along(df)

      .copy <- .self$copy()
      .copy$types   <- c(types_before, types_to_add)
      .copy$ind_col <- c(
        .self$ind_col[ !(names(.self$ind_col) %in% names(df)) ],
        set_names(new_ind_col, names(df))
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

    ## Need this when modifying $types (or $backingfile)
    init_address = function() {
      .self$extptr <- getXPtrFDF(.self$backingfile, .self$nrow_all, .self$types)
    },

    ## Need this as data mask
    as_env = function(parent = parent.frame()) {
      name_inds <- names(inds <- .self$ind_col)
      e <- new.env(parent = parent, size = length(inds) + 10L)
      ## lapply() is mandatory so that all 'i' has not always the same value
      lapply(inds, function(i) {
        delayedAssign(name_inds[i], pull(.self, i), assign.env = e)
      })
      ## Make sure code doesn't access all columns in memory
      delayedAssign("ERROR_DELAYED", stop("ERROR NOT DELAYED"), assign.env = e)
      e
    },

    ## When printing
    show = function() {
      cat(sprintf(
        "# A%s Filebacked Data Frame with %s rows and %s columns.\n",
        `if`(.self$is_grouped, " grouped", ""), .self$nrow, .self$ncol))
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
setMethod("dim", signature(x = "FDF"), function(x) c(x$nrow, x$ncol))

#' @rdname FDF-methods
#' @export
setMethod("length", signature(x = "FDF"), function(x) x$ncol)

################################################################################
