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
  base_dir <- path(pkg, dir)
  dir_create(base_dir)

  proj <- make_project(pkg)

  write_project(proj, base_dir)

  invisible()
}
