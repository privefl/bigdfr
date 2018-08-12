################################################################################

drop_ext <- function(file) tools::file_path_sans_ext(file)

################################################################################

#' Read a file
#'
#' Read a file as a Filebacked Data Frame by using package {bigreadr}.
#'
#' @param file File to read.
#' @param filter Vector used to subset the rows of each data frame.
#' @param backingfile Path to the file storing the data frame on disk.
#'   Default uses `file` without its extension.
#' @inheritDotParams bigreadr::big_fread2 -file -.transform -.combine
#'
#' @return A Filebacked Data Frame.
#'
#' @export
#'
FDF_read <- function(file,
                     filter = NULL,
                     backingfile = drop_ext(file),
                     ...) {

  # Prepare reading
  X <- NULL
  init <- TRUE
  fill_FDF <- function(df) {

    # Filter rows
    if (!is.null(filter)) df <- df[filter, , drop = FALSE]
    # Initialize resulting FDF on first round, then add columns
    if (init) {
      X <<- FDF(df, backingfile = backingfile)
      init <<- FALSE
    } else {
      X <<- X$add_columns(df)
    }
    NULL
  }

  # Read and fill by parts
  bigreadr::big_fread2(file, .transform = fill_FDF, .combine = unlist, ... = ...)

  # Returns
  X
}

################################################################################
