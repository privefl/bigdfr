################################################################################

#' @export
#' @importFrom dplyr tibble
dplyr::tibble

################################################################################

as_dplyr_groups <- function(tbl, x) {

  attr <- attributes(tbl)

  attr$class <- c("grouped_df", attr$class)

  groups <- x$groups
  groups$.rows <- lapply(groups$ind_row, function(ind) match_int(ind, x$ind_row))
  groups$ind_row <- NULL
  attr$groups <- groups

  attributes(tbl) <- attr
  tbl
}

################################################################################

#' Convert to tibble
#'
#' @param x A [FDF][FDF-class].
#' @param ... Not used.
#'
#' @importFrom dplyr as_tibble
#' @export
#' @method as_tibble FDF
#'
#' @rdname as_tibble
#'
#' @examples
#' test <- FDF(datasets::iris)
#' test2 <- filter(test, Species == "setosa")
#' as_tibble(test2)
as_tibble.FDF <- function(x, ...) {

  assert_nodots()

  tbl <- as_tibble(
    lapply(x$colnames, function(col) {
      pull(x, col)
    })
  )

  `if`(x$is_grouped, as_dplyr_groups(tbl, x), tbl)
}

################################################################################

#' @exportMethod as_tibble
#' @rdname as_tibble
setGeneric("as_tibble", dplyr::as_tibble)

################################################################################
