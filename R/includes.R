get_includes <- function() {
  inc_dir <- path_package("hyderogen", "inst", "includes")
  path_rel(dir_ls(inc_dir, recursive = TRUE, type = "file"), inc_dir)
}

include <- function(...) {
  i_path <- path_package("hyderogen", "inst", "includes", ...)

  structure(
    list(
      content = paste(readLines(i_path), collapse = "\n"),
      rel_path = path(...)
    ),
    class = "include"
  )
}

print.include <- function(x, ...) {
  cat(x$content)
  invisible(x)
}
