################################################################################

AUTHORIZED_TYPES <-
  c("numeric" = 8L, "integer" = 4L, "factor" = 2L, "character" = 2L)

ERROR_TYPE <- "Some column types are not authorized."

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

    #### Active bindings
    address = function() {
      if (identical(.self$extptr, methods::new("externalptr"))) { # nil
        .self$extptr <- getXPtrFDF(.self$backingfile, .self$nrow, .self$types)
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
      .self$types       <- AUTHORIZED_TYPES[coltypes]

      ## Add columns and fill them with data
      add_bytes(.self$backingfile, .self$nrow * sum(.self$types))

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
