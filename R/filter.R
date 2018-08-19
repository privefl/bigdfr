################################################################################

verif_lgl <- function(x, n, i) {

  if (is.logical(x) && length(x) %in% c(1, n)) return(x)

  stop2("Expression #%d does not evaluate to a logical vector of length 1 or %d.", i, n)
}

################################################################################

#' @inherit dplyr::filter title description return params
#'
#' @param .data A [FDF][FDF-class].
#'
#' @importFrom dplyr filter
#' @export
#' @method filter FDF
#'
#' @rdname filter
#'
#' @examples
#' (test <- FDF(datasets::iris))
#' filter(test, Species == "setosa")
#' filter(test, Species == "setosa", Sepal.Length < 5)
filter.FDF <- function(.data, ...) {

  dots <- quos(...)
  if (length(dots) == 0) return(.data)

  groups <- .data$groups
  list_ind_row <- groups$ind_row
  # groups$ind_row <- NULL

  # Get some variables in memory
  names_involved <- unique(unlist(lapply(dots, get_call_names)))
  names_to_get <- intersect(.data$colnames, names_involved)
  names_pulled <- lapply(set_names(names_to_get), function(var_name) {
    extract_var(.data, var_name, list_ind_row)
  })

  indices <- lapply(seq_along(list_ind_row), function(k) {

    indices_k <- list_ind_row[[k]]
    names_pulled_group_k <- lapply(names_pulled, function(x) x[[k]])

    list_bool <- lapply(seq_along(dots), function(i) {
      e <- as_env(names_pulled_group_k, parent = quo_get_env(dots[[i]]))
      quo_set_env(dots[[i]], e) %>%
        eval_tidy() %>%
        verif_lgl(length(indices_k), i)
    })

    indices_k[Reduce('&', list_bool)]
  })

  if (.data$is_grouped) {
    groups$ind_row <- indices
    .data$copy(ind_row = intersect(.data$ind_row, unlist(indices)),
               groups_internal = groups[lengths(indices) != 0, ])
  } else {
    .data$copy(ind_row = indices[[1]])
  }
}

################################################################################

#' @export
#'
#' @param subset Integer vector to (further) subset `.data$ind_row`.
#' @param check Whether to check `subset`? Default is `TRUE`.
#'
#' @rdname filter
#'
#' @examples
#' filter_int(test, 1:50)
filter_int <- function(.data, subset, check = TRUE) {

  ind_row_filtered <- .data$ind_row[subset]
  if (check && anyNA(ind_row_filtered))
    stop2("'subset' must have values between 1 and %d", .data$nrow)

  if (.data$is_grouped) {
    ## TODO: optimize
    groups <- .data$groups
    groups$ind_row <- lapply(groups$ind_row, function(ind) {
      ind_rel <- match(ind, ind_row_filtered)
      has_match <- !is.na(ind_rel)
      ind[has_match][order(ind_rel[has_match])]
    })
    .data$copy(ind_row = ind_row_filtered,
               groups_internal = groups[lengths(groups$ind_row) != 0, ])
  } else {
    .data$copy(ind_row = ind_row_filtered)
  }
}

################################################################################

#' @exportMethod filter
#' @rdname filter
setGeneric("filter", dplyr::filter)

################################################################################
