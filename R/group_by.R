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

  .copy <- .data$copy()
  if (!add) .copy$groups_internal <- tibble()

  groups <- .copy$groups
  list_ind_row <- groups$ind_row
  groups$ind_row <- NULL

  # Get grouping variables in memory
  var_names <- select.FDF(.copy, ...)$colnames
  names_pulled <- lapply(var_names, function(var_name) {
    extract_var(.copy, var_name, list_ind_row)
  })

  # Let {dplyr} do the hard work
  list_info_groups <- lapply(seq_along(list_ind_row), function(k) {
    names_pulled_group_k <- lapply(names_pulled, function(x) x[[k]])
    grouped_df <- dplyr::group_by_at(as_tibble(names_pulled_group_k),
                                     names(names_pulled_group_k))
    attributes(grouped_df)[c("labels", "indices")]
  })

  # Get relative grouping indices and make them absolute
  list_list_ind <- lapply(list_info_groups, function(x) x$indices)
  sizes <- sapply(list_list_ind, length)
  list_ind <- unlist(list_list_ind, recursive = FALSE)
  rel_to_abs(list_ind_row, list_ind, sizes)

  # Bind previous groups, new groups and corresponding indices
  .copy$groups_internal <- dplyr::bind_cols(
    groups[rep(seq_along(sizes), sizes), ],
    do.call(dplyr::bind_rows, lapply(list_info_groups, function(x) x$labels)),
    tibble(ind_row = list_ind)
  )

  .copy
}

################################################################################

#' @exportMethod group_by
#' @rdname group_by
setGeneric("group_by", dplyr::group_by)

################################################################################
