################################################################################

#' @inherit dplyr::select title description return
#'
#' @param .data A [FDF][FDF-class].
#' @inheritParams tidyselect::vars_select
#'
#' @importFrom dplyr select
#' @importFrom tidyselect vars_select quos
#' @export
#' @method select FDF
#'
#' @rdname select
#'
#' @examples
#' (test <- FDF(datasets::iris))
#' select(test, 1:4)
#' select(test, -5)
#' select(test, c("Sepal.Length", "Sepal.Width"))
#' select(test, Sepal.Length, Sepal.Width)
select.FDF <- function(.data, ...) {

  ind_vars <- .data$ind_col
  var_names <- vars_select(names(ind_vars), !!!quos(...))

  new_data <- .data$copy()
  new_data$ind_col <- ind_vars[var_names]
  new_data$init_address()
}

################################################################################

#' @exportMethod select
#' @rdname select
setGeneric("select", dplyr::select)

#' @rdname select
setMethod("select", "FDF", select.FDF)

################################################################################
