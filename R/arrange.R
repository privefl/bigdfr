################################################################################

arrange.FDF <- function(.data, order) {

  assert_lengths(.data$ind_row, order)
  ind_row_arranged <- .data$ind_row[order]
  new_data <- .data$copy()
  new_data$ind_row <- ind_row_arranged
  new_data$init_address()
  new_data
}

################################################################################

setGeneric("arrange", dplyr::arrange)

#' @inherit dplyr::arrange title description return
#'
#' @param .data A [FDF][FDF-class].
#' @param order Integer vector or orders for `.data$ind_row`.
#'
#' @importFrom dplyr arrange
#' @export
#'
#' @examples
#' test <- FDF(datasets::iris)
#' pull(test, 3)
#' order <- order(pull(test, 3))
#' test2 <- arrange(test, order)
#' pull(test2, 3)
#' test3 <- filter(test, 1:50)
#' pull(test3, 2)
#' test4 <- arrange(test3, order(pull(test3, 2)))
#' pull(test4, 2)
setMethod("arrange", "FDF", arrange.FDF)

################################################################################
