#' @importFrom fs path path_package path_file path_ext path_ext_remove
#'   dir_exists dir_delete dir_create path_rel dir_ls dir_copy dir_walk dir_map
#'   file_create file_exists file_copy file_move file_delete
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
#' @param assets A character vector specifying file paths of assets to include
#'   in the jekyll assets folder. Javascript files are sorted into `assets/js/`,
#'   CSS files are sorted into `assets/css`, and all other files are put directly
#'   into `assets/`.
#'
#' @export
jekyll <- function(path = ".", dir = "docs", build = FALSE, assets = NULL) {
  if (!dir_exists(path)) {
    stop(
      "invalid `jekyll()` argument, `path` file path does not exist",
      call. = FALSE
    )
  }

  blocks <- parse_package(path)

  path_docs <- path(path, dir)

  if (dir_exists(path_docs)) {
    dir_walk(path_docs, file_delete)
  } else {
    dir_create(path_docs)
  }

  create_folders(path_docs, blocks)
  create_files(path_docs, blocks)

  if (!build) {
    return(invisible(TRUE))
  }

  copy_config(path_docs)
  copy_includes(path_docs)
  copy_layouts(path_docs)
  copy_plugins(path_docs)
  copy_assets(path_docs, assets)

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

copy_config <- function(path, assets) {
  file_copy(path_jekyll("configs/default.yaml"), path(path, "_config.yaml"))
}

copy_layouts <- function(path) {
  file_move(dir_copy(path_jekyll("layouts"), path), path(path, "_layouts"))
}

copy_includes <- function(path) {
  file_move(dir_copy(path_jekyll("includes"), path), path(path, "_includes"))
}

copy_plugins <- function(path) {
  file_move(dir_copy(path_jekyll("plugins"), path), path(path, "_plugins"))
}

copy_assets <- function(path, extras = NULL) {
  dir_assets <- dir_create(path(path, "assets"))

  # sass
  file_move(dir_copy(path_jekyll("sass"), path), path(path, "_sass"))

  dir_css <- dir_create(path(dir_assets, "css"))
  path_main_css <- file_create(path(dir_css, "main.scss"))

  sass_files <- dir_map(path_jekyll("sass"), path_file)
  sass_imports <- glue("@import \"{ path_ext_remove(sass_files) }\";")
  cat(
    file = path_main_css,
    sep = "\n",
    "---",
    "---",
    sass_imports
  )

  # js
  dir_js <- dir_create(path(dir_assets, "js"))

  # extras
  path_extras <- map(extras, function(path_extra) {
    if (path_ext(path_extra) == "css") {
      file_copy(path_extra, dir_css)
    } else if (path_ext(path_extra) == "js") {
      file_copy(path_extra, dir_js)
    } else {
      file_copy(path_file(path_extra), dir_assets)
    }
  })

  if (length(path_extras) > 0) {
    path_extras <- map(path_extras, function(p) {
      list(
        ext = path_ext(p),
        path = paste0("/", path_rel(p, path))
      )
    })

    cat(
      file = path(path, "_config.yaml"),
      sep = "\n",
      append = TRUE,
      "",
      as_yaml(
        list(
          hyderogen = list(
            assets = path_extras
          )
        )
      )
    )
  }
}

path_jekyll <- function(path) {
  system.file("jekyll", path, package = "hyderogen")
}
