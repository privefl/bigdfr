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
#' @export
#' @method filter FDF
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

  list_bool <- lapply(seq_along(dots), function(i) {
    e <- .data$as_env(parent = quo_get_env(dots[[i]]))
    quo_set_env(dots[[i]], e) %>%
      eval_tidy() %>%
      verif_lgl(.data$nrow, i)
  })

  filter_int(.data, subset = which(Reduce('&', list_bool)), check = FALSE)
}

################################################################################

#' @export
#'
#' @param subset Integer vector to (further) subset `.data$ind_row`.
#' @param check Whether to check `subset`? Default is `TRUE`.
#'
#' @rdname filter
#'
#' @examples
#' filter_int(test, 1:50)
filter_int <- function(.data, subset, check = TRUE) {

  ind_row_filtered <- .data$ind_row[subset]
  if (check && anyNA(ind_row_filtered))
    stop2("'subset' must have values between 1 and %d", .data$nrow)

  .data$copy(ind_row = ind_row_filtered)
}

################################################################################

#' @exportMethod filter
#' @rdname filter
setGeneric("filter", dplyr::filter)

#' #' @rdname filter
#' setMethod("filter", "FDF", filter.FDF)

################################################################################
