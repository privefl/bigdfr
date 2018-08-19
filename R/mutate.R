################################################################################

#' @inherit dplyr::mutate title description return params
#'
#' @param .data A [FDF][FDF-class].
#'
#' @importFrom dplyr mutate
#' @export
#' @method mutate FDF
#'
#' @rdname mutate
#'
#' @examples
#' (test <- FDF(datasets::airquality))
#' mutate(test, Temp_Celsius = (Temp - 32) / 1.8,
#'        Temp_Kelvin = Temp_Celsius + 273.15)
mutate.FDF <- function(.data, ...) {

  name_dots <- names(dots <- quos(..., .named = TRUE))
  if (length(dots) == 0) return(.data)

  # list_ind_row <- .data$groups$ind_row
  #
  # e_new <- list()
  # for (i in seq_along(dots)) {
  #
  #   quo_i <- dots[[i]]
  #   parent_env <- quo_get_env(quo_i)
  #   names_involved <- get_call_names(quo_i)
  #   names_to_get <- setdiff(intersect(.data$colnames, names_involved), names(e_new))
  #
  #   names_pulled <- lapply(set_names(names_to_get), function(var_name) {
  #     extract_var(.data, var_name, list_ind_row)
  #   })
  #
  #   e_new[[name_dots[i]]] <- slapply(seq_along(list_ind_row), function(k) {
  #     names_pulled_group_k <- lapply(names_pulled, function(x) x[[k]])
  #     e <- list2env(names_pulled_group_k, parent = parent_env)
  #     eval_tidy(quo_set_env(quo_i, list2env(groups, parent = e)))
  #   })
  # }

  e_new <- list()
  for (i in seq_along(dots)) {
    e <- .data$as_env(parent = quo_get_env(dots[[i]]))
    e_new[[name_dots[i]]] <- dots[[i]] %>%
      quo_set_env(as_env(e_new, parent = e)) %>%
      eval_tidy()
  }

  e_new[name_dots] %>%
    as.data.frame() %>%
    .data$add_columns()
}

################################################################################

#' @exportMethod mutate
#' @rdname mutate
setGeneric("mutate", dplyr::mutate)

################################################################################
