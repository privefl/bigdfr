################################################################################

filter.FDF <- function(.data, subset) {

  ind_row_filtered <- .data$ind_row[subset]
  new_data <- .data$copy()
  new_data$ind_row <- ind_row_filtered
  new_data$init_address()
  new_data
}

################################################################################

setGeneric("filter", dplyr::filter)

#' @inherit dplyr::filter title description return
#'
#' @param .data A [FDF][FDF-class].
#' @param subset Logical or integer vector to further subset `.data$ind_row`.
#'
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' (test <- FDF(datasets::iris))
#' filter(test, 1:50)
#' filter(test, c(TRUE, FALSE, FALSE))
setMethod("filter", "FDF", filter.FDF)

################################################################################
