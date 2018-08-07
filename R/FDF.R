################################################################################

AUTHORIZED_TYPES <-
  c("numeric" = 8L, "integer" = 4L, "factor" = 2L, "character" = 2L)

ERROR_TYPE <- "Some column types are not authorized."

################################################################################

#' Class FBM
#'
#' A reference class for storing and accessing data frames stored on disk.
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
    is_saved    = "logical",

    #### Active bindings
    address = function() {
      if (identical(.self$extptr, methods::new("externalptr"))) { # nil
        .self$extptr <- getXPtrFDF(.self$backingfile, .self$nrow, .self$types)
      }
      .self$extptr
    },

    ncol = function() length(types)
  ),

  methods = list(

    initialize = function(df, backingfile) {

      assert_pos(nrow(df))
      coltypes <- sapply(df, class)
      assert_pos(length(coltypes))
      if (!all(coltypes %in% names(AUTHORIZED_TYPES))) stop2(ERROR_TYPE)

      .self$backingfile <- create_file(backingfile)
      .self$is_saved    <- FALSE
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
#' @export
#'
FDF <- function(df, backingfile = tempfile()) {
  assert_class(df, "data.frame")
  do.call(methods::new, args = c(Class = "FDF", as.list(environment())))
}

################################################################################
