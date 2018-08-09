################################################################################

#' There is NO accessor for class `FDF`.
#'
#' @param x A [FDF][FDF-class] object.
#'
#' @include FDF.R
#'
#' @name FDF-accessors
NULL

#' @export
#' @rdname FDF-accessors
setMethod('[', "FDF", function(x) stop2("Accessor '[' does NOT exist for an FDF object.\n  Please use 'select' and/or 'filter' instead."))

#' @export
#' @rdname FDF-accessors
setMethod('[[', "FDF", function(x) stop2("Accessor '[[' does NOT exist for an FDF object.\n  Please use 'pull' instead."))


################################################################################
