################################################################################

pull.FDF <- function(.data, var) {

  if (.data$types[[var]] == 2) {
    extract_string(.data$address, var, .data$strings)
  } else {
    extract_numeric(.data$address, var)
  }
}

################################################################################

setGeneric("pull", dplyr::pull)

#' @inherit dplyr::pull title description return
#'
#' @param .data A [FDF][FDF-class].
#' @param var For now, ONE index of the column you want to extract.
#'
#' @importFrom dplyr pull
#' @export
#'
#' @examples
#' test <- FDF(datasets::iris)
#' pull(test, 1)
#' pull(test, 5)
setMethod("pull", "FDF", pull.FDF)

################################################################################
