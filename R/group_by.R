################################################################################

#' @export
#' @method ungroup FDF
#'
#' @param x A [FDF][FDF-class].
#' @param ... Not used.
#'
#' @rdname group_by
ungroup.FDF <- function(x, ...) {
  assert_nodots()
  x$copy(groups_internal = tibble())
}

#' @exportMethod ungroup
#' @rdname group_by
setGeneric("ungroup", dplyr::ungroup)

################################################################################

split_var <- function(.data, rel_var_name, list_ind_row) {

  glob_ind_var <- .data$ind_col[[rel_var_name]]
  addr <- .data$address

  new_list <- switch(
    names(.data$types)[glob_ind_var],
    Date      = ,
    POSIXt    = ,
    numeric   = split_dbl(addr, glob_ind_var, list_ind_row),
    logical   = ,
    integer   = split_int(addr, glob_ind_var, list_ind_row),
    character = {
      uniq <- .data$meta[[glob_ind_var]]$uniq
      split_ushort(addr, glob_ind_var, list_ind_row,
                   match_chr(.data$strings, uniq) - 1L, length(uniq))
    },
    factor    = {
      uniq <- .data$meta[[glob_ind_var]]$levels
      split_ushort(addr, glob_ind_var, list_ind_row,
                   match_chr(.data$strings, uniq) - 1L, length(uniq))
    },
    stop2(ERROR_TYPE)
  )
  new_list <- unlist(new_list, recursive = FALSE)
  new_list[lengths(new_list) != 0]
}

################################################################################

#' @inherit dplyr::group_by title
#'
#' @inheritParams select.FDF
#' @param add When `add = FALSE`, the default, `group_by()` will override
#'   existing groups. To add to the existing groups, use `add = TRUE`.
#'
#' @importFrom dplyr group_by
#' @export
#' @method group_by FDF
#'
#' @rdname group_by
#'
#' @examples
#' (test <- FDF(datasets::iris))
#' (test2 <- group_by(test, Species))
#' test2$groups
#' group_by(test, starts_with("Sepal"))$groups
#' group_by(test2, Sepal.Length, add = TRUE)$groups
group_by.FDF <- function(.data, ..., add = FALSE) {

  if (length(quos(...)) == 0)
    stop2("You must group by at least one variable.")

  # Get grouping variables and group indices
  group_names <- select.FDF(.data, ...)$colnames
  list_ind_row <- `if`(add, .data$groups$ind_row, list(.data$ind_row))
  for (var in group_names) {
    list_ind_row <- split_var(.data, var, list_ind_row)
  }

  # Get group variables
  if (add) group_names <- c(head(names(.data$groups), -1), group_names)
  data_groups <- select(.data, group_names)
  data_groups$groups_internal = tibble()
  data_groups$ind_row = sapply(list_ind_row, function(x) x[1])
  groups <- as_tibble(data_groups)
  groups$ind_row <- list_ind_row

  .data$copy(groups_internal = dplyr::arrange_at(groups, -ncol(groups)))
}

################################################################################

#' @exportMethod group_by
#' @rdname group_by
setGeneric("group_by", dplyr::group_by)

################################################################################
