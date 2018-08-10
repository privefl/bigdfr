################################################################################

#' Convert to tibble
#'
#' @param x A [FDF][FDF-class].
#' @param ... Not used.
#'
#' @importFrom dplyr as_tibble
#' @export
#'
#' @rdname as_tibble
#'
#' @examples
#' test <- FDF(datasets::iris)
#' test2 <- filter(test, Species == "setosa")
#' as_tibble(test2)
as_tibble.FDF <- function(x, ...) {

  assert_nodots()

  dplyr::as_data_frame(
    lapply(x$colnames, function(col) {
      pull(x, col)
    })
  )
}

################################################################################

#' @export
#' @rdname as_tibble
setGeneric("as_tibble", dplyr::as_tibble)

#' @rdname as_tibble
setMethod("as_tibble", "FDF", as_tibble.FDF)

################################################################################
