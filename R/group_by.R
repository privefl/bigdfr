################################################################################

utils::globalVariables(c("NESTED", "rel_ind_row"))

################################################################################

#' @inherit dplyr::group_by title
#'
#' @inheritParams select.FDF
#' @param add When `add = FALSE`, the default, `group_by()` will override
#'   existing groups. To add to the existing groups, use `add = TRUE`.
#'
#' @importFrom dplyr group_by tibble
#' @importFrom rlang :=
#' @export
#' @method group_by FDF
#'
#' @rdname group_by
#'
#' @examples
#' (test <- FDF(datasets::iris))
#' (test2 <- group_by(test, Species))
#' test2$groups
#' group_by(test, Sepal.Length, Sepal.Width)
#' group_by(test, starts_with("Sepal"))
group_by.FDF <- function(.data, ..., add = FALSE) {

  if (length(quos(...)) == 0)
    stop2("You must group by at least one variable.")

  var_names <- select.FDF(.data, ...)$colnames
  data <- .data

  ind_row <- data$ind_row
  current_groups <- `if`(!add || nrow(data$groups) == 0,
                         tibble(rel_ind_row = list(seq_along(ind_row))),
                         data$groups)

  for (name in var_names) {

    current_groups <- current_groups %>%
      mutate(NESTED = lapply(rel_ind_row, function(ind) {
        .part <- data$copy(ind_row = ind_row[ind])
        by <- pull(.part, name)
        u_by <- sort(unique(by))
        tibble(!!rlang::sym(name) := u_by,
               rel_ind_row = lapply(u_by, function(x) ind[by == x]))
      })) %>%
      select(-rel_ind_row) %>%
      tidyr::unnest(NESTED)
  }

  data$copy(groups = current_groups)
}

################################################################################

#' @exportMethod group_by
#' @rdname group_by
setGeneric("group_by", dplyr::group_by)

#' @rdname group_by
setMethod("group_by", "FDF", group_by.FDF)

################################################################################
