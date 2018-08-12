################################################################################

#' Convert to a tibble of the n first/last elements.
#'
#' @param x A [FDF][FDF-class].
#' @param ... Not used.
#' @param n A single positive integer.
#'
#' @importFrom utils head
#' @export
#'
#' @include filter.R
#'
#' @rdname head
#'
#' @examples
#' test <- FDF(datasets::iris)
#' head(test)
head.FDF <- function(x, n = 6L, ...) {

  assert_nodots()
  assert_pos(n)

  n <- min(n, x$nrow)

  as_tibble(filter_int(x, seq_len(n), check = FALSE))
}

################################################################################

#' @export
#' @rdname head
setGeneric("head", utils::head)

#' @rdname head
setMethod("head", "FDF", head.FDF)

################################################################################

#' @importFrom utils tail
#' @export
#'
#' @rdname head
#'
#' @examples
#' test <- FDF(datasets::iris)
#' tail(test)
tail.FDF <- function(x, n = 6L, ...) {

  assert_nodots()
  assert_pos(n)

  n <- min(n, x$nrow)

  as_tibble(filter_int(x, x$nrow + seq_len(n) - n, check = FALSE))
}

################################################################################

#' @export
#' @rdname head
setGeneric("tail", utils::tail)

#' @rdname head
setMethod("tail", "FDF", tail.FDF)

################################################################################
