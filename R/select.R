################################################################################

#' @inherit dplyr::select title description return
#'
#' @param .data A [FDF][FDF-class].
#' @param subset
#'   Logical, integer or character vector to further subset `.data$ind_col`.
#'
#' @importFrom dplyr select
#' @export
#'
#' @rdname select
#'
#' @examples
#' (test <- FDF(datasets::iris))
#' select(test, 1:4)
#' select(test, -5)
#' select(test, c(TRUE, FALSE))
#' select(test, c("Sepal.Length", "Sepal.Width"))
select.FDF <- function(.data, subset) {

  ind_col_filtered <- .data$ind_col[subset]
  new_data <- .data$copy()
  new_data$ind_col <- ind_col_filtered
  new_data$init_address()
  new_data
}

################################################################################

#' @export
#' @rdname select
setGeneric("select", dplyr::select)

#' @rdname select
setMethod("select", "FDF", select.FDF)

################################################################################