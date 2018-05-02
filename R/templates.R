get_templates <- function() {
  tem_dir <- path_package("hyderogen", "inst", "templates")
  path_rel(dir_ls(tem_dir, recursive = TRUE, type = "file"), tem_dir)
}

template <- function(...) {
  t_path <- path_package("hyderogen", "inst", "templates", ...)

  structure(
    list(
      content = paste(readLines(t_path), collapse = "\n"),
      rel_path = path(...)
    ),
    class = "template"
  )
}

print.template <- function(x, ...) {
  cat(x$content)
  invisible(x)
}
