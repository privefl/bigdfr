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
#' @importFrom magrittr %<>%
#' @export
#' @method summarise FDF
#'
#' @rdname summarize
#'
#' @return A tibble with group values and summarized values.
#'
#' @examples
#' test <- FDF(datasets::iris)
#' test %>%
#'   group_by(Species) %>%
#'   summarize(mean = mean(Sepal.Length))
#'
#' # You can also get a list-column
#' test %>%
#'   group_by(Species) %>%
#'   summarize(range = range(Sepal.Length))
summarise.FDF <- function(.data, ...) {

  name_dots <- names(dots <- quos(...))
  data <- .data
  groups <- data$groups

  for (i in seq_along(dots)) {

    parent_env <- quo_get_env(dots[[i]])

    groups %<>%
      mutate(
        !!sym(name_dots[i]) := slapply(rel_ind_row, function(ind) {
          .copy <- data$copy(ind_row = data$ind_row[ind])
          e <- .copy$as_env(parent = parent_env)
          dots[[i]] %>%
            quo_set_env(as_env(groups, parent = e)) %>%
            eval_tidy()
        })
      )
  }

  select(groups, -rel_ind_row)
}

################################################################################

#' @exportMethod summarize
#' @rdname summarize
setGeneric("summarize", dplyr::summarize)

#' @exportMethod summarise
#' @rdname summarize
setGeneric("summarise", dplyr::summarise)

################################################################################
