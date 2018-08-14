#' @useDynLib bigdfr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang := quos eval_tidy quo_get_env quo_set_env set_names
#' @keywords internal
"_PACKAGE"

as_env <- rlang::as_environment
