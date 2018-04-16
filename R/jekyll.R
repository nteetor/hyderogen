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
#' @details
#'
#' Collections are currently overwritten. However, if a family's name is
#' changed then the old, corresponding collection will not be deleted. This is
#' to prevent custom collections from accidentally being removed. This behaviour
#' is subject to change.
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

  base_dir <- path(pkg, dir)

  dir_create(base_dir)

  proj <- parse_files(r_dir)

  copy_structure(desc::desc(pkg), dir = base_dir)

  # "project" might as well be "package" at this point
  write_project(proj, dir = base_dir)

  invisible()
}
