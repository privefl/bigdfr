################################################################################

get_fct <- function(.data, name, ind_row) {

  glob_ind_var <- .data$ind_col[[name]]
  type <- names(.data$types)[glob_ind_var]
  fct <- switch(
    type,
    numeric   = extract_fct_dbl(.data$address, glob_ind_var, ind_row),
    integer   = extract_fct_int(.data$address, glob_ind_var, ind_row),
    logical   = extract_fct_int(.data$address, glob_ind_var, ind_row),
    character = extract_fct_ushort(.data$address, glob_ind_var, ind_row),
    stop2(ERROR_TYPE)
  ) ## levels and integers

  lvl <- fct[[1]]
  if (type == "logical") {
    lvl <- as.logical(lvl)
  } else if (type == "character") {
    lvl <- .data$strings[lvl + 1L]
  }

  list(int = fct[[2]], lvl = lvl)
}

################################################################################

utils::globalVariables(c("NESTED", "ind_row"))

################################################################################

#' @inherit dplyr::group_by title
#'
#' @inheritParams select.FDF
#' @param add When `add = FALSE`, the default, `group_by()` will override
#'   existing groups. To add to the existing groups, use `add = TRUE`.
#'
#' @importFrom dplyr group_by
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
#' group_by(test, Sepal.Length, Sepal.Width)$groups
#' group_by(test, starts_with("Sepal"))$groups
group_by.FDF <- function(.data, ..., add = FALSE) {

  if (length(quos(...)) == 0)
    stop2("You must group by at least one variable.")

  var_names <- select.FDF(.data, ...)$colnames
  data <- .data

  ind_row <- data$ind_row
  current_groups <- `if`(add && data$is_grouped, data$groups,
                         tibble(ind_row = list(data$ind_row)))

  for (name in var_names) {

    current_groups <- current_groups %>%
      mutate(NESTED = lapply(ind_row, function(ind) {
        fct <- get_fct(data, name, ind)
        splt <- split(ind, structure(fct$int, class = "factor", levels = fct$lvl))
        tibble(!!sym(name) := fct$lvl, ind_row = splt)
      })) %>%
      select(-ind_row) %>%
      tidyr::unnest(NESTED)
  }

  data$copy(groups = current_groups)
}

################################################################################

#' @exportMethod group_by
#' @rdname group_by
setGeneric("group_by", dplyr::group_by)

################################################################################
