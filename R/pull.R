################################################################################

#' @inherit dplyr::pull title description return
#'
#' @param .data A [FDF][FDF-class].
#' @inheritParams tidyselect::vars_pull
#'
#' @importFrom dplyr pull
#' @importFrom tidyselect vars_pull enquo
#' @export
#'
#' @rdname pull
#'
#' @examples
#' test <- FDF(datasets::iris)
#' pull(test, 1)
#' pull(test, 5)
#' pull(test, Species)
pull.FDF <- function(.data, var = -1) {

  ind_vars <- .data$ind_col
  var_name <- vars_pull(names(ind_vars), !!enquo(var))
  ind_var <- ind_vars[[var_name]]

  if (.data$types[[ind_var]] == 2) {
    extract_string(.data$address, ind_var, .data$strings)
  } else {
    extract_numeric(.data$address, ind_var)
  }
}

################################################################################

#' @export
#' @rdname pull
setGeneric("pull", dplyr::pull)

#' @rdname pull
setMethod("pull", "FDF", pull.FDF)

################################################################################
