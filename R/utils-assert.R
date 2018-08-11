################################################################################

# printf   <- function(...) cat(    sprintf(...))
message2 <- function(...) message(sprintf(...))
# warning2 <- function(...) warning(sprintf(...), call. = FALSE)
stop2    <- function(...) stop(   sprintf(...), call. = FALSE)

################################################################################

# # ARGS
# assert_args <- function(f, args.name) {
#
#   if (!inherits(f, "function"))
#     stop2("'%s' is not a function.", deparse(substitute(f)))
#
#   if (!all(args.name %in% names(formals(f))))
#     stop2("'%s' should have argument%s named %s.",
#           deparse(substitute(f)),
#           `if`(length(args.name) > 1, "s", ""),
#           toString(args.name))
# }

################################################################################

# # NUMBER OF CORES
# assert_cores <- function(ncores) {
#   if (ncores > getOption("bigstatsr.ncores.max")) {
#     stop2(paste0("You are trying to use more cores than allowed.",
#                  " We advise you to use `nb_cores()`.\n",
#                  "You can change this default value with",
#                  " `options(bigstatsr.ncores.max = Inf)`."))
#   }
# }

################################################################################

assert_lengths <- function(...) {
  lengths <- lengths(list(...))
  if (length(lengths) > 1) {
    if (any(diff(lengths) != 0))
      stop2("GET_ERROR_DIM()")
  } else {
    stop2("You should check the lengths of at least two elements.")
  }
}

################################################################################

# # INTEGERS
# assert_int <- function(x) {
#   if (!is.null(x) && any(x != trunc(x)))
#     stop2("'%s' should contain only integers.", deparse(substitute(x)))
# }

################################################################################

# POSITIVE INDICES
assert_pos <- function(x)  {
  if (!all(x > 0))
    stop2("'%s' should have only positive values.", deparse(substitute(x)))
}

################################################################################

# # TYPEOF
# assert_type <- function(x, type)  {
#   if (typeof(x) != type)
#     stop2("'%s' is not of type '%s'.", deparse(substitute(x)), type)
# }

################################################################################

# CLASS
assert_class <- function(x, class)  {
  if (!inherits(x, class))
    stop2("'%s' is not of class '%s'.", deparse(substitute(x)), class)
}

################################################################################

# # ALL SAME VALUE
# assert_all <- function(x, value) {
#   if (any(x != value))
#     stop2("At least one value of '%s' is different from '%s'",
#           deparse(substitute(x)), value)
# }

################################################################################

# DIRECTORY
assert_dir <- function(dir.path) {
  if (!dir.exists(dir.path)) {
    if (dir.create(dir.path)) {
      message2("Creating directory \"%s\" which didn't exist..", dir.path)
    } else {
      stop2("Problem creating directory \"%s\". Recursive path?", dir.path)
    }
  }
}

################################################################################

# FILE EXISTS
assert_noexist <- function(file) {
  if (file.exists(file))
    stop2("File '%s' already exists.", file)
}

# assert_exist <- function(file) {
#   if (!file.exists(file))
#     stop2("File '%s' doesn't exist.", file)
# }

################################################################################

# EXTENSION
assert_ext <- function(file, ext) {
  if (!grepl(sprintf("\\.%s$", ext), file))
    stop2("Extension of '%s' must be '.%s'.", file, ext)
}

################################################################################

# ... not used
assert_nodots <- function() {

  list_dots <- eval(parse(text = "list(...)"), parent.frame())
  if (!identical(list_dots, list()))
    stop2("Argument '%s' not used.", names(list_dots[1]))
}

################################################################################
