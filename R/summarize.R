################################################################################

slapply <- function(X, FUN) {
  res <- lapply(X, FUN)
  `if`(all(lengths(res) == 1), unlist(res, recursive = FALSE), res)
}

################################################################################

#' @inherit dplyr::summarize title description params
#'
#' @param .data A [FDF][FDF-class].
#'
#' @importFrom dplyr summarize summarise
#' @export
#' @method summarise FDF
#'
#' @rdname summarize
#'
#' @return A tibble with group values and summarized values.
#'
#' @include utils-eval.R
#'
#' @examples
#' test <- FDF(datasets::iris)
#' test %>%
#'   group_by(Species) %>%
#'   summarize(mean = mean(Sepal.Length), n = n())
#'
#' # You can also get a list-column
#' test %>%
#'   group_by(Species) %>%
#'   summarize(range = range(Sepal.Length))
summarise.FDF <- function(.data, ...) {

  name_dots <- names(dots <- quos(..., .named = TRUE))

  groups <- .data$groups
  list_ind_row <- groups$ind_row
  groups$ind_row <- NULL
  group_vars_after <- head(names(groups), -1)

  for (i in seq_along(dots)) {

    quo_i <- dots[[i]]
    parent_env <- quo_get_env(quo_i)
    n_defined <- find_n(parent_env)

    names_involved <- get_call_names(quo_i)
    names_to_get <- setdiff(intersect(.data$colnames, names_involved), names(groups))

    names_pulled <- lapply(set_names(names_to_get), function(var_name) {
      extract_var(.data, var_name, list_ind_row)
    })

    groups[[name_dots[i]]] <- slapply(seq_along(list_ind_row), function(k) {
      names_pulled_group_k <- lapply(names_pulled, function(x) x[[k]])
      e <- list2env(names_pulled_group_k, parent = parent_env)
      q <- quo_modif(quo_i, n_defined, val = length(list_ind_row[[k]]), env = e)
      eval_tidy(q, data = groups)
    })
  }

  dplyr::group_by_at(groups, group_vars_after)
}

################################################################################

#' @exportMethod summarize
#' @rdname summarize
setGeneric("summarize", dplyr::summarize)

#' @exportMethod summarise
#' @rdname summarize
setGeneric("summarise", dplyr::summarise)

################################################################################
