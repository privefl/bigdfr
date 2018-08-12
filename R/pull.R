################################################################################

#' @inherit dplyr::pull title description return
#'
#' @param .data A [FDF][FDF-class].
#' @inheritParams tidyselect::vars_pull
#'
#' @importFrom dplyr pull
#' @importFrom tidyselect vars_pull enquo
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

  rel_ind_vars <- .data$ind_col
  rel_var_name <- vars_pull(names(rel_ind_vars), !!enquo(var))
  glob_ind_var <- rel_ind_vars[[rel_var_name]]

  switch(names(.data$types)[glob_ind_var],
         numeric   = extract_dbl(.data$address, glob_ind_var),
         integer   = extract_int(.data$address, glob_ind_var),
         logical   = extract_lgl(.data$address, glob_ind_var),
         character = extract_string(.data$address, glob_ind_var, .data$strings),
         stop2(ERROR_TYPE))
}

################################################################################

#' @exportMethod pull
#' @rdname pull
setGeneric("pull", dplyr::pull)

#' @rdname pull
setMethod("pull", "FDF", pull.FDF)

################################################################################
