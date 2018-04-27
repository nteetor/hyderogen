.templates <- list(
  index = "index.md",
  about = "about.md",
  `404` = "404.html",
  config = "_config.yaml",
  `docs/index` = "docs/index.md"
)

.layouts <- list(
  home = "home.html",
  page = "page.html",
  docs = "docs.html",
  collection = "collection.html"
)

.includes <- list(
  header = "header.html",
  `page-sidebar` = "page-sidebar.html",
  `page-navbar` = "page-navbar.html"
)

path_template <- function(name) {
  if (is.null(.templates[[name]])) {
    stop("`path_template()`, unknown template ", '"', name, '"', call. = FALSE)
  }

  system.file("templates", .templates[[name]], package = "hyderogen")
}

path_layout <- function(name) {
  if (is.null(.layouts[[name]])) {
    stop("`path_layout()`, unknown layout ", '"', name, '"', call. = FALSE)
  }

  system.file("layouts", .layouts[[name]], package = "hyderogen")
}

path_include <- function(name) {
  if (is.null(.includes[[name]])) {
    stop('`path_include()`, unknown include "', name, '"', call. = FALSE)
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
  filled <- fill_template(path_template(name), data)

  t_path <- path(dir, .templates[[name]])

  dir_create(path_dir(t_path))
  file_create(t_path)

  cat(filled, "\n", file = t_path)

  invisible()
}

copy_layout <- function(name, dir) {
  file_copy(path_layout(name), path(dir, .layouts[[name]]), overwrite = TRUE)
}

copy_include <- function(name, dir) {
  file_copy(path_include(name), path(dir, .includes[[name]]), overwrite = TRUE)
}
