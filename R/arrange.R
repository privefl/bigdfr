################################################################################

#' @importFrom dplyr desc
#' @export
dplyr::desc

################################################################################

arrange_by_group <- function(.data, dots, method) {

  if (!.data$is_grouped) stop2("'.data' has no group..")

  groups <- .data$groups
  list_ind_row <- groups$ind_row

  # Get some variables in memory
  names_involved <- unique(unlist(lapply(dots, get_call_names)))
  names_to_get <- intersect(.data$colnames, names_involved)
  names_pulled <- lapply(set_names(names_to_get), function(var_name) {
    extract_var(.data, var_name, list_ind_row)
  })

  indices <- lapply(seq_along(list_ind_row), function(k) {

    indices_k <- list_ind_row[[k]]
    names_pulled_group_k <- lapply(names_pulled, function(x) x[[k]])

    list_vec <- lapply(seq_along(dots), function(i) {
      e <- list2env(names_pulled_group_k, parent = quo_get_env(dots[[i]]))
      eval_tidy(quo_set_env(dots[[i]], e))
    })
    order <- do.call(base::order, c(list_vec, list(method = method)))
    assert_lengths(indices_k, order)

    indices_k[order]
  })

  groups$ind_row <- indices
  .data$copy(ind_row = unlist(indices),
             groups_internal = groups)
}

################################################################################

#' @inherit dplyr::arrange title return params
#'
#' @param .data A [FDF][FDF-class].
#' @inheritParams base::order
#'
#' @importFrom dplyr arrange
#' @export
#' @method arrange FDF
#'
#' @rdname arrange
#'
#' @include filter.R
#'
#' @examples
#' test <- FDF(datasets::iris)
#' test2 <- arrange(test, Sepal.Length)
#' test3 <- arrange(test, Species, desc(Sepal.Length))
arrange.FDF <- function(.data, ..., .by_group = FALSE, method = "radix") {

  dots <- quos(...)
  if (length(dots) == 0) return(.data)
  if (.by_group) return(arrange_by_group(.data, dots, method))

  list_vec <- lapply(dots, function(q) {
    e <- .data$as_env(parent = quo_get_env(q))
    eval_tidy(quo_set_env(q, e))
  })
  order <- do.call(base::order, c(list_vec, list(method = method)))
  assert_lengths(.data$ind_row, order)

  filter_int(.data, subset = order, check = FALSE)
}

################################################################################

#' @exportMethod arrange
#' @rdname arrange
setGeneric("arrange", dplyr::arrange)

################################################################################
