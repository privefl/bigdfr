################################################################################

#' @inherit dplyr::select title description return
#'
#' @param .data A [FDF][FDF-class].
#' @inheritParams tidyselect::vars_select
#'
#' @importFrom dplyr select
#' @importFrom tidyselect vars_select vars_select_helpers
#' @export
#' @method select FDF
#'
#' @rdname select
#'
#' @examples
#' (test <- FDF(datasets::iris))
#' select(test, 1:4)
#' select(test, -5)
#' select(test, -Species)
#' select(test, c("Sepal.Length", "Sepal.Width"))
#' select(test, Sepal.Length, Sepal.Width)
#' select(test, starts_with("Sepal"))
select.FDF <- function(.data, ...) {

  dots <- lapply(quos(...), rlang::env_bury, !!!vars_select_helpers)

  ind_vars <- .data$ind_col
  var_names <- vars_select(names(ind_vars), !!!dots)

  .data$copy(ind_col = ind_vars[var_names])
}

################################################################################

#' @exportMethod select
#' @rdname select
setGeneric("select", dplyr::select)

################################################################################
