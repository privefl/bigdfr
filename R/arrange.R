################################################################################

#' @inherit dplyr::arrange title return params
#'
#' @param .data A [FDF][FDF-class].
#'
#' @importFrom dplyr arrange quos
#' @export
#' @method arrange FDF
#'
#' @rdname arrange
#'
#' @include filter.R
#'
#' @examples
#' test <- FDF(datasets::iris)
#' test2 <- arrange(test, Sepal.Length)
arrange.FDF <- function(.data, ...) {

  dots <- quos(...)
  order <- seq_len(.data$nrow)

  assert_lengths(.data$ind_row, order)
  filter_int(.data, subset = order, check = FALSE)
}

################################################################################

#' @exportMethod arrange
#' @rdname arrange
setGeneric("arrange", dplyr::arrange)

#' @rdname arrange
setMethod("arrange", "FDF", arrange.FDF)

################################################################################
