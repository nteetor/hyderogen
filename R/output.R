write_out <- function(x, ...) {
  UseMethod("write_out")
}

write_out.page <- function(x, ...) {
  if (!length(attr(x, "path"))) {
    # message("skipping ", x$roxygen$filename)
    return(invisible())
  }

  dest <- path(getwd(), attr(x, "path"))
  dir_create(path_dir(dest))

  content <- c("---\n", as_yaml(x), "---\n")

  tryCatch(
    cat(content, file = dest, sep = ""),
    error = function(e) {
      message("failed to create page ", dest)
    }
  )

  invisible()
}

write_out.template <- function(x, ...) {
  filled <- glue::glue(x$content, .envir = parent.frame())

  dest <- path(getwd(), x$rel_path)
  dir_create(path_dir(dest))

  tryCatch(
    cat(filled, file = dest),
    error = function(e) {
      message("failed to create template ", dest)
    }
  )

  invisible()
}

write_out.layout <- function(x, ...) {
  dest <- path(getwd(), "_layouts", x$rel_path)
  dir_create(path_dir(dest))

  tryCatch(
    cat(x$content, file = dest),
    error = function(e) {
      message("failed to create layout ", dest)
    }
  )
}

write_out.include <- function(x, ...) {
  dest <- path(getwd(), "_includes", x$rel_path)
  dir_create(path_dir(dest))

  tryCatch(
    cat(x$content, file = dest),
    error = function(e) {
      message("failed to create include ", dest)
    }
  )
}
