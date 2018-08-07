create_file <- function(file) {

  file <- path.expand(file)
  assert_noexist(file)
  assert_dir(dirname(file))
  if (!file.create(file))
    stop2("Problem while create file '%s'.", file)

  normalizePath(file)
}
