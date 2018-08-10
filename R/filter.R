################################################################################

verif_lgl <- function(x, n, i) {

  if (is.logical(x) && length(x) %in% c(1, n)) return(x)

  stop2("Expression #%d does not evaluate to a logical vector of length 1 or %d.", i, n)
}

################################################################################

#' @inherit dplyr::filter title description return params
#'
#' @param .data A [FDF][FDF-class].
#'
#' @importFrom dplyr filter
#' @importFrom rlang quos quo_get_env quo_set_env eval_tidy
#' @export
#'
#' @rdname filter
#'
#' @examples
#' (test <- FDF(datasets::iris))
#' filter(test, Species == "setosa")
#' filter(test, Species == "setosa", Sepal.Length < 5)
filter.FDF <- function(.data, ...) {

  dots <- quos(...)
  if (length(dots) == 0) return(.data)

  e <- .data$as_env(parent = quo_get_env(dots[[1]]))
  list_bool <- lapply(seq_along(dots), function(i) {
    quo_set_env(dots[[i]], e) %>%
      eval_tidy() %>%
      verif_lgl(.data$nrow, i)
  })

  filter_int(.data, subset = which(Reduce('&', list_bool)))
}

################################################################################

#' @export
#'
#' @param subset Integer vector to (further) subset `.data$ind_row`.
#'
#' @rdname filter
#'
#' @examples
#' filter_int(test, 1:50)
filter_int <- function(.data, subset) {
  ind_row_filtered <- .data$ind_row[subset]
  new_data <- .data$copy()
  new_data$ind_row <- ind_row_filtered
  new_data$init_address()
}

################################################################################

#' @export
#' @rdname filter
setGeneric("filter", dplyr::filter)

#' @rdname filter
setMethod("filter", "FDF", filter.FDF)

################################################################################
