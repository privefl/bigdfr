################################################################################

as_env <- rlang::as_environment

################################################################################

get_call_names <- function(call) {
  if (rlang::is_quosure(call)) {
    unlist(get_call_names(rlang::quo_get_expr(call)))
  } else if (is.call(call)) {
    unname(lapply(as.list(call)[-1], get_call_names))
  } else if (is.name(call)) {
    as.character(call)
  } else {
    NULL
  }
}

################################################################################
