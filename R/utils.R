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
