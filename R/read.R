################################################################################

drop_ext <- function(file) tools::file_path_sans_ext(file)

################################################################################

#' Read a file
#'
#' Read a file as a Filebacked Data Frame by using package {bigreadr}.
#'
#' @param filter Vector used to subset the rows of each data frame.
#' @param backingfile Path to the file storing the data frame on disk.
#'   Default uses `file` without its extension.
#' @inheritParams bigreadr::big_fread2
#' @param ... More arguments to be passed to [data.table::fread].
#'
#' @return A Filebacked Data Frame of with <nrow> rows and <length(select)> columns.
#'
#' @export
#'
FDF_read <- function(file, select = NULL,
                     nb_parts = NULL,
                     filter = NULL,
                     backingfile = drop_ext(file),
                     skip = 0,
                     progress = FALSE,
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
  bigreadr::big_fread2(
    file, nb_parts, skip = skip, select = select, progress = progress,
    .transform = fill_FDF, .combine = unlist, ... = ...)

  # Returns
  X
}

################################################################################
