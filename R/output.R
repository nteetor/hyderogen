write_project <- function(proj, dir = "docs/") {

  colls_dir <- path(dir, "collections")
  dir_create(colls_dir)
  walk(proj$collections, write_collection, dir = colls_dir)

  invisible(proj)
}

format_project <- function(proj) {
  map(proj$collections, format_collection)
}

write_collection <- function(coll, dir) {
  if (!dir_exists(dir)) {
    stop("`write_collection()`, `dir` does not exist", call. = FALSE)
  }

  if (!length(coll$name)) {
    stop(
      "`write_collection()`, collection is missing a name",
      call. = FALSE
    )
  }

  coll_path <- path(dir, paste0("_", coll$name))

  if (dir_exists(coll_path)) {
    dir_delete(coll_path)
  }

  dir_create(coll_path)

  walk(coll$items, write_item, dir = coll_path)

  invisible(coll)
}

format_collection <- function(coll) {
  map(coll$items, format_item)
}

write_item <- function(item, dir) {
  if (!length(item$name)) {
    stop(
      "`write_item()`, collection item is missing a name",
      call. = FALSE
    )
  }

  item_path <- path(dir, item$name)
  path_ext(item_path) <- "md"

  write_front_matter(format_item(item), item_path)
}

format_item <- function(item) {
  as.yaml(item$roxygen, indent.mapping.sequence = TRUE)
}

write_front_matter <- function(yaml, file, preserve = FALSE, newline = "\n") {
  if (!file_exists(file)) {
    file_create(file)
  }

  if (!file_access(file, c("read", "write"))) {
    stop(
      "`write_front_matter()`, must have read and write access to `file`",
      call. = FALSE
    )
  }

  if (preserve) {
    stop(
      "`write_font_matter()`, `preserve = TRUE` is not implemented",
      call. = FALSE
    )

    if (file_exists(file)) {

      lines <- trimws(readLines(file), "both")
      lines <- lines[lines != ""]

      if (lines[1] == "---") {

      }
    }
  }

  if (file_exists(file)) {
    lines <- readLines(file)
  } else {
    lines <- ""
  }

  cat("---", yaml, "---", lines, sep = newline, file = file)

  invisible(file)
}
