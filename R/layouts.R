get_layouts <- function() {
  lay_dir <- path_package("hyderogen", "inst", "layouts")
  path_rel(dir_ls(lay_dir, recursive = TRUE, type = "file"), lay_dir)
}

layout <- function(...) {
  l_path <- path_package("hyderogen", "inst", "layouts", ...)

  structure(
    list(
      content = paste(readLines(l_path), collapse = "\n"),
      rel_path = path(...)
    ),
    class = "layout"
  )
}

print.layout <- function(x, ...) {
  cat(x$content)
  invisible(x)
}
