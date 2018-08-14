################################################################################

#' @inherit dplyr::summarize title description params
#'
#' @param .data A [FDF][FDF-class].
#'
#' @importFrom dplyr summarize
#' @importFrom magrittr %<>%
#' @export
#' @method summarize FDF
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
summarize.FDF <- function(.data, ...) {

  name_dots <- names(dots <- quos(...))
  data <- .data
  groups <- data$groups

  for (i in seq_along(dots)) {

    parent_env <- quo_get_env(dots[[i]])

    groups %<>%
      mutate(
        !!sym(name_dots[i]) := sapply(rel_ind_row, function(ind) {
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

#' @rdname summarize
setMethod("summarize", "FDF", summarize.FDF)

################################################################################
