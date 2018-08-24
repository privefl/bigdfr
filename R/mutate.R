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

  list_ind_row <- .data$groups$ind_row

  e_new <- list()
  for (i in seq_along(dots)) {

    quo_i <- dots[[i]]
    parent_env <- quo_get_env(quo_i)
    n_defined <- find_n(parent_env)

    names_involved <- get_call_names(quo_i)
    names_to_get <- setdiff(intersect(.data$colnames, names_involved), names(e_new))

    names_pulled <- lapply(set_names(names_to_get), function(var_name) {
      extract_var(.data, var_name, list_ind_row)
    })

    e_new[[name_dots[i]]] <- lapply(seq_along(list_ind_row), function(k) {
      names_pulled_group_k <- lapply(names_pulled, function(x) x[[k]])
      e <- list2env(names_pulled_group_k, parent = parent_env)
      q <- quo_modif(quo_i, n_defined, val = length(list_ind_row[[k]]), env = e)
      eval_tidy(q, data = lapply(e_new, function(x) x[[k]]))
    })
  }

  cols_to_add <- lapply(e_new[name_dots], unlist)
  if (.data$is_grouped) {
    rel_ind_row <- match_int(.data$ind_row, unlist(list_ind_row))
    cols_to_add <- lapply(cols_to_add, function(x) x[rel_ind_row])
  }
  .data$add_columns(as_tibble(cols_to_add))
}

################################################################################

#' @exportMethod mutate
#' @rdname mutate
setGeneric("mutate", dplyr::mutate)

################################################################################
