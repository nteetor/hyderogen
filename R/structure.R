.templates <- list(
  index = "index.md",
  about = "about.md",
  `404` = "404.html",
  config = "_config.yaml"
)

.layouts <- list(
  home = "home.html",
  page = "page.html"
)

.includes <- list(
  header = "header.html",
  `page-sidebar` = "page-sidebar.html",
  `page-navbar` = "page-navbar.html"
)

get_template <- function(name) {
  if (is.null(.templates[[name]])) {
    stop("`get_template()`, unknown template ", '"', name, '"', call. = FALSE)
  }

  system.file("templates", .templates[[name]], package = "hyderogen")
}

get_layout <- function(name) {
  if (is.null(.layouts[[name]])) {
    stop("`get_layout()`, unknown layout ", '"', name, '"', call. = FALSE)
  }

  system.file("layouts", .layouts[[name]], package = "hyderogen")
}

get_include <- function(name) {
  if (is.null(.includes[[name]])) {
    stop('`get_include()`, unknown include "', name, '"', call. = FALSE)
  }

  system.file("includes", .includes[[name]], package = "hyderogen")
}

fill_template <- function(file, data = list()) {
  content <- paste0(readLines(file), collapse = "\n")

  env <- list2env(data, parent = emptyenv())

  glue_hyderogen(env, content)
}

fill_front_matter <- function(file, data = list()) {
  matter <- get_front_matter(file)

  env <- list2env(data, parent = emptyenv())

  glue_hyderogen(env, matter)
}

copy_template <- function(name, dir, data = list()) {

  filled <- fill_template(get_template(name), data)
  t_path <- path(dir, .templates[[name]])

  file_create(t_path)
  cat(filled, "\n", file = t_path)

  invisible()
}

copy_layout <- function(name, dir) {
  layout <- get_layout(name)
  file_copy(layout, path(dir, path_file(layout)), overwrite = TRUE)
}

copy_include <- function(name, dir) {
  include <- get_include(name)
  file_copy(include, path(dir, path_file(include)), overwrite = TRUE)
}

copy_structure <- function(dir) {

  copy_template("index", dir)
  copy_template("404", dir)

  dir_create(path(dir, "_includes"))
  copy_include("header", path(dir, "_includes"))
  copy_include("page-sidebar", path(dir, "_includes"))
  copy_include("page-navbar", path(dir, "_includes"))

  dir_create(path(dir, "_layouts"))
  copy_layout("page", path(dir, "_layouts"))
  copy_layout("home", path(dir, "_layouts"))

  invisible()
}

copy_config <- function(dir, pkg = desc::desc(), collections = NULL) {
  if (length(collections)) {
    coll_names <- map_chr(collections, "name")

    colls <- map(collections, ~ {
      list(
        output = TRUE,
        permalink = glue("/docs/{ pkg$get('Version') }/:collection/:path")
      )
    })

    names(colls) <- coll_names

    collections <- as.yaml(list(collections = colls))
  }

  copy_template("config", dir, list(
    title = pkg$get("Title"),
    description = pkg$get("Description"),
    package = pkg$get("Package"),
    version = pkg$get("Version"),
    collections = collections
  ))
}
