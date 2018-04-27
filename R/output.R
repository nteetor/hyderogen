write_project <- function(proj, dir) {
  write_layouts(proj, dir)
  write_includes(proj, dir)
  write_templates(proj, dir)
  write_collections(proj, dir)

  invisible(proj)
}

write_templates <- function(proj, dir) {
  dat <- list(
    package = proj$package$name,
    version = proj$package$version,
    title = proj$package$title,
    description = proj$package$description,
    username = proj$username,
    collections = as.yaml(list(
      collections = set_names(
        map(proj$collections, ~ list(output = .$output, permalink = .$permalink)),
        map(proj$collections, "name")
      )
    ))
  )

  for (t in proj$templates) {
    copy_template(t, dir, data = dat)
  }
}

write_layouts <- function(proj, dir) {
  l_dir <- path(dir, "_layouts")
  dir_create(l_dir)

  for (l in proj$layouts) {
    copy_layout(l, l_dir)
  }
}

write_includes <- function(proj, dir) {
  i_dir <- path(dir, "_includes")
  dir_create(i_dir)

  for (i in proj$includes) {
    copy_include(i, i_dir)
  }
}

format_project <- function(proj) {
  map(proj$collections, format_collection)
}

write_collections <- function(proj, dir) {
  c_dir <- path(dir, "collections")
  dir_create(c_dir)

  for (c in proj$collections) {
    c_path <- path(c_dir, paste0("_", c$name))

    if (dir_exists(c_path)) {
      dir_delete(c_path)
    }

    dir_create(c_path)

    for (i in c$items) {
      write_item(i, c_path)
    }
  }
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

  item_path <- path_ext_set(path(dir, item$name), "md")

  write_front_matter(format_item(item), item_path)
}

format_item <- function(item) {
  jekyll_matter <- list(
    layout = item$layout
  )

  as.yaml(
    c(jekyll_matter, item$roxygen),
    indent.mapping.sequence = TRUE
  )
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
