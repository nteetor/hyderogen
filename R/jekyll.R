#' Build a jekyll site
#'
#' The `jekyll` function builds a jekyll site from your
#' package's roxygen.
#'
#' @param pkg A character string specifying a file path to an R package,
#'   defaults to `"."`, the current directory.
#'
#' @param dir A folder path, relative to `pkg`, where the jekyll site will be
#'   built, defaults to `"docs"`.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' jekyll("~/git/mypkg")
#'
#' }
#'
jekyll <- function(pkg = ".", dir = "docs") {
  r_dir <- path(pkg, "R")

  if (!dir_exists(r_dir)) {
    stop(
      "`jekyll()`, no R/ directory found",
      call. = FALSE
    )
  }

  proj <- parse_files(r_dir)

  if (dir == stdout()) {
    cat(format_project(dir), sep = "\n")
  } else {
    write_project(proj, dir = path(pkg, dir))
  }

  invisible()
}
