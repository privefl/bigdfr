################################################################################

as_env <- rlang::as_environment

################################################################################

var_used <- function(of_data, in_quo) {

  in_quo <- quo(Sepal.Length + Sepal.Width)
  e_test <- of_data$copy(ind_row = 1L)$as_env()
  eval_tidy(quo_set_env(in_quo, e_test))

  keep <- sapply(names(iris), function(name) {
    myquo <- quo(pryr::promise_info(!!sym(name))$evaled)
    eval_tidy(quo_set_env(myquo, env = e_test))
  })
  names(keep)[keep]
}

################################################################################
