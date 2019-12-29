cat0 <- function(..., sep = "") {
  cat(..., sep = sep)
}

`%&&%` <- function(a, b) {
  if (!is.null(a) && !is.na(a) && !isFALSE(a)) {
    b
  } else {
    NULL
  }
}

block_path_file <- function(block) {
  if (!is.null(block %@% "call") && block %@% "call" == "_PACKAGE") {
    "index"
  } else {
    tolower(block[["name"]])
  }
}

list_gather <- function(x, key) {
  purrr::reduce(x, .init = list(), function(acc, obj) {
    if (is.null(obj)) {
      return(acc)
    }

    k <- obj[[key]]
    obj[[key]] <- NULL

    if (is.null(acc[[k]])) {
      acc[[k]] <- list()
    }

    acc[[k]] <- c(acc[[k]], list(obj))

    acc
  })
}

list_unpack <- function(x) {
  purrr::modify_if(
    x,
    ~ rlang::is_bare_list(.x) && length(.x) == 1,
    ~ .x[[1]]
  )
}
