jekyll <- function(pkg = ".", dir = "docs") {
  r_dir <- path(pkg, "R")

  if (!dir_exists(r_dir)) {
    stop(
      "`jekyll()`, no R/ directory found",
      call. = FALSE
    )
  }

  if (dir != stdout() && !dir_exists(dir)) {
    dir_create(dir)
  }

  proj <- parse_files(r_dir)

  if (dir == stdout()) {
    cat(format_project(dir), sep = "\n")
  } else {
    write_project(proj, dir = dir)
  }

  invisible()
}
