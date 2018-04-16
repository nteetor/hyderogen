.templates <- list(
  index = "index.md",
  about = "about.md",
  `404` = "404.html",
  config = "_config.yaml"
)

.layouts <- list(
  default = "default.html",
  page = "page.html"
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

copy_layout <- function(name, dir, data = list()) {
  stop("`copy_layout()` is not implemented")
}

copy_structure <- function(meta, dir) {

  copy_template("config", dir, list(
    title = meta$get("Title"),
    description = meta$get("Description")
  ))

  copy_template("index", dir)
  copy_template("404", dir)

  invisible()
}
