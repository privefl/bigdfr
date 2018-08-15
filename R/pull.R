################################################################################

extract_var <- function(.data, rel_var_name,
                        list_ind_row = list(.data$ind_row)) {

  glob_ind_var <- .data$ind_col[[rel_var_name]]
  addr <- .data$address

  switch(
    names(.data$types)[glob_ind_var],
    numeric   = extract_dbl(addr, glob_ind_var, list_ind_row),
    integer   = extract_int(addr, glob_ind_var, list_ind_row),
    logical   = extract_lgl(addr, glob_ind_var, list_ind_row),
    character = extract_string(addr, glob_ind_var, list_ind_row, .data$strings),
    stop2(ERROR_TYPE)
  )
}

#' @inherit dplyr::pull title description return
#'
#' @param .data A [FDF][FDF-class].
#' @inheritParams tidyselect::vars_pull
#'
#' @importFrom dplyr pull
#' @importFrom tidyselect vars_pull
#' @export
#' @method pull FDF
#'
#' @rdname pull
#'
#' @examples
#' test <- FDF(datasets::iris)
#' pull(test, 1)
#' pull(test, 5)
#' pull(test, Species)
pull.FDF <- function(.data, var = -1) {
  extract_var(.data, vars_pull(.data$colnames, !!rlang::enquo(var)))[[1]]
}

################################################################################

#' @exportMethod pull
#' @rdname pull
setGeneric("pull", dplyr::pull)

################################################################################
