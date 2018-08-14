################################################################################

#' @importFrom dplyr desc
#' @export
dplyr::desc

################################################################################

#' @inherit dplyr::arrange title return params
#'
#' @param .data A [FDF][FDF-class].
#'
#' @importFrom dplyr arrange
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
#' test3 <- arrange(test, Species, desc(Sepal.Length))
arrange.FDF <- function(.data, ...) {

  dots <- quos(...)
  if (length(dots) == 0) return(.data)

  list_vec <- lapply(dots, function(q) {
    e <- .data$as_env(parent = quo_get_env(q))
    eval_tidy(quo_set_env(q, e))
  })
  order <- do.call(base::order, list_vec)

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
