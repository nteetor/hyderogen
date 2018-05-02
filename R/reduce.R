page_reduce <- function(x, y, ...) {
  UseMethod("page_reduce")
}

page_reduce.list <- function(x, y, ...) {
  if (!inherits(y, "page")) {
    browser()
  }

  if (page_name(y) %in% map_chr(x, page_name)) {
    tryCatch(
      map_if(x, ~ page_name(.) == page_name(y), ~ page_merge(., y)),
      error = function(e) {
        browser()
      }
    )

  } else {
    c(x, list(y))
  }
}

page_reduce.page <- function(x, y, ...) {
  if (page_name(x) == page_name(y)) {
    return(list_merge(x, y))
  }

  list(x, y)
}

page_merge <- function(x, y, ...) {
  UseMethod("page_merge")
}

page_merge.page_set <- function(x, y, ...) {

}

page_merge.page <- function(x, y, ...) {
  structure(
    list(
      layout = "page-set",
      roxygen = list(

      )
    )
  )
}

page_name <- function(x) {
  stopifnot(inherits(x, "page"))
  x$roxygen$rdname %||% x$roxygen$name
}


