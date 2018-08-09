################################################################################

#' @inherit dplyr::filter title description return
#'
#' @param .data A [FDF][FDF-class].
#' @param subset Logical or integer vector to further subset `.data$ind_row`.
#'
#' @importFrom dplyr filter
#' @export
#'
#' @rdname filter
#'
#' @examples
#' (test <- FDF(datasets::iris))
#' filter(test, 1:50)
#' filter(test, c(TRUE, FALSE, FALSE))
filter.FDF <- function(.data, subset) {

  ind_row_filtered <- as.integer(stats::na.omit(.data$ind_row[subset]))
  new_data <- .data$copy()
  new_data$ind_row <- ind_row_filtered
  new_data$init_address()
  new_data
}

################################################################################

#' @export
#' @rdname filter
setGeneric("filter", dplyr::filter)

#' @rdname filter
setMethod("filter", "FDF", filter.FDF)

################################################################################
