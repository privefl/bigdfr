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

  name_dots <- names(dots <- quos(...))
  if (length(dots) == 0) return(.data)

  e_new <- list()
  for (i in seq_along(dots)) {
    e <- .data$as_env(parent = quo_get_env(dots[[i]]))
    e_new[[name_dots[i]]] <- dots[[i]] %>%
      quo_set_env(as_env(e_new, parent = e)) %>%
      eval_tidy()
  }

  e_new[name_dots] %>%
    as.data.frame() %>%
    .data$add_columns()
}

################################################################################

#' @exportMethod mutate
#' @rdname mutate
setGeneric("mutate", dplyr::mutate)

#' @rdname mutate
setMethod("mutate", "FDF", mutate.FDF)

################################################################################
