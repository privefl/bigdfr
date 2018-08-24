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

find_n <- function(env) {
  found <- methods::findFunction("n", where = env)
  (length(found) > 0) && is.null(attr(found[[1]], "name"))
}

repl_call_n <- function(call, val) {

  if (is.call(call)) {
    if (identical(call, call("n"))) {
      val
    } else {
      as.call(lapply(call, repl_call_n, val = val))
    }
  } else {
    call
  }
}

quo_modif <- function(quo, n_defined, val, env = quo_get_env(quo)) {

  expr <- rlang::quo_get_expr(quo)
  if (!n_defined) expr <- repl_call_n(expr, val)

  rlang::as_quosure(expr, env)
}

################################################################################
