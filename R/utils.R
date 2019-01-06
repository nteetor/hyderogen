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
