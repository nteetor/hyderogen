#' @importFrom fs path path_package dir_exists dir_delete dir_create dir_ls
#'                dir_copy file_create file_exists file_copy file_move
#' @importFrom glue glue glue_collapse double_quote
#' @importFrom purrr walk %||%
NULL

#' Jekyll folder structure
#'
#' Create a jekyll site structure from a package's roxygen. Optionally, a
#' site may be built, too. Building your site requires
#' [jekyll](https://jekyllrb.com/docs/installation/) to be installed and
#' available on your system.
#'
#' @param path A file path specifying the folder of a package, defaults to
#'   `"."`.
#'
#' @param dir A file path, relative to `path`, specifying the destination of the
#'   jekyll folders and files, defaults to `"docs/"`.
#'
#' @param build One of `TRUE` or `FALSE` specifying if the jekyll site is built
#'   after creating the folder structure, defaults to `FALSE`.
#'
#' @export
jekyll <- function(path = ".", dir = "docs", build = FALSE) {
  if (!dir_exists(path)) {
    stop(
      "invalid `jekyll()` argument, `path` file path does not exist",
      call. = FALSE
    )
  }

  blocks <- parse_package(path)

  path_docs <- path(path, dir)

  if (dir_exists(path_docs)) {
    dir_delete(path_docs)
  }

  dir_create(path_docs)

  create_folders(path_docs, blocks)
  create_files(path_docs, blocks)

  if (!build) {
    return(invisible(TRUE))
  }

  copy_includes(path_docs)
  copy_layouts(path_docs)
  copy_config(path_docs)

  args <- c(
    "build"
  )

  invisible(processx::run("jekyll", args, wd = path_docs))
}

create_folders <- function(path, blocks) {
  walk(blocks, function(block) {
    block[["family"]] %&&% dir_create(path(path, block[["family"]]))
  })
}

create_files <- function(path, blocks) {
  walk(blocks, function(block) {
    block_file <- glue("{ tolower(block[['name']]) }.md")

    block[["layout"]] <- "doc"

    block_path <- block[["family"]] %&&%
      file_create(path(path, block[["family"]], block_file)) %||%
      file_create(path(path, block_file))

    cat0(
      file = block_path,
      "---\n",
      as_yaml(block),
      "---\n"
    )
  })
}

copy_config <- function(path) {
  file_copy(path(path_configs(), "default.yaml"), path(path, "_config.yaml"))
}

copy_layouts <- function(path) {
  file_move(dir_copy(path_layouts(), path), path(path, "_layouts"))
}

copy_includes <- function(path) {
  file_move(dir_copy(path_includes(), path), path(path, "_includes"))
}

path_configs <- function() {
  system.file("jekyll", "configs", package = "hyderogen")
}

path_layouts <- function() {
  system.file("jekyll", "layouts", package = "hyderogen")
}

path_includes <- function() {
  system.file("jekyll", "includes", package = "hyderogen")
}
